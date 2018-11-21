(ns mongofinil.core
  "A Mongoid-like library that lets you focus on the important stuff"
  (:require [somnium.congomongo :as congo]
            [somnium.congomongo.coerce :as congo-coerce :refer [*translations*]]
            [mongofinil.validation :as mv]
            [mongofinil.validation-helpers :as mvh]
            [clojure.string :as str])

  (:use [mongofinil.helpers :only (assert! throw-if-not throw-if ref? throwf eager-map)])
  (:import org.bson.types.ObjectId
           (org.joda.time DateTime
                          DateTimeZone)))

(defn col-validators
  "Returns a vector of validators from field definitions"
  [fields]
  (->> fields
       (eager-map :validator)
       (filter identity)

       ;; validator for the require attribute
       (into [(mvh/require-keys (eager-map :name (filter :required fields)))])))

(defn apply-to-last [f args]
  (throw-if-not (seq? args) "Expected seq, got %s" args)
  (let [skipped (butlast args)
        val (last args)
        val (f val)
        args (concat skipped [val])]
    args))

(defn wrap-transients
  "Take the last arguent and strips the transient attributes from it. Apply the
  DB operation, and then readdd the transient attributes to its result"
  [f transients]
  (if transients
    (fn [& args]
      (throw-if-not (-> args first map?) "Expecting map, got %s" (first args))
      (let [val (first args)
            rest (rest args)
            transient (select-keys val transients)
            transiented (apply dissoc val transients)
            args (concat [transiented] rest)
            results (apply f args)
            _ (throw-if-not (seq? results) "expected seq")
            results (map #(when % (merge transient %)) results)]
        results))
    f))

(defn wrap-refs
  "If input? and output?, replace the first argument's value with the result. If
  just input?, deref the only argument. If just output?, wrap a ref around the
  result. Otherwise, just run normally"
  [f input? output?]
  (cond
   (and input? output?) (fn [& args]
                          (let [first (first args)
                                first (if (ref? first) first (ref first))
                                derefed (concat [@first] (rest args))
                                results (apply f derefed)
                                result (last results)] ;; NOT FIRST, that returns nil
                            (throw-if-not (seq? results) "expecting seq")
                            (throw-if-not (= 1 (count results)) "expecting exactly one member")
                            (dosync (ref-set first result))
                            (list first))) ;; must always return lists
   output? (fn [& args]
             (let [results (apply f args)]
               (throw-if-not (seq? results) "expected seq")
               (map #(when %
                       (ref %)) results)))
   input? (fn [& args]
            (throw-if-not (-> args first ref?) "Expecting ref, got %s" (first args))
            (apply f @(first args) (rest args)))
   :else f))

(defn- only-opts-true-keys
  "Construct a set from opts given to :only.

`only-opts` is either a sequence or a map where all values are true (excluding :_id).

see: https://docs.mongodb.org/manual/tutorial/project-fields-from-query-results/

note:

    You cannot combine inclusion and exclusion semantics in a single
    projection with the exception of the _id field.
"
  [only-opts]
  (if (map? only-opts)
    (if (-> only-opts
            (dissoc :_id)
            vals
            (->> (every? identity)))
      (-> (keys only-opts)
          set
          (disj :_id))
      #{})
    (set only-opts)))

(defn apply-defaults
  [defaults row only]
  (let [true-keys (only-opts-true-keys only)]
    (if (empty? defaults)
      row
      (do
        (throw-if-not (map? row) "Expected a hash, got %s" row)
        (reduce (fn [r d]
                  (let [[k v] d]
                    (if (and (seq true-keys)
                             (not (contains? true-keys k)))
                      ;; dont apply defaults if not in the 'only set
                      r
                      (assoc r k
                             (cond
                               (contains? r k) (get r k)
                               (fn? v) (v r)
                               :else v)))))
                row defaults)))))

(defn extract-congo-argument
  "Given an argument list, extract the keyword argument named 'key'"
  [args key]
  (let [args (if (-> args count odd?) (rest args) args)
        map (apply hash-map args)]
    (get map key)))

(defn wrap-output-defaults
  "Wrap f to add default output values for the result of f"
  [f defaults]
  (if defaults
    (fn [& args]
      (let [results (apply f args)
            only (extract-congo-argument args :only)]
        (throw-if-not (seq? results) "expected seq")
        (map
         (fn [result]
           (when result
             (apply-defaults defaults result only)))
         results)))
    f))

(defn wrap-output-incomplete?
  "Wrap f to add metadata about whether the returned document is incomplete"
  [f]
  (fn [& args]
    (let [results (apply f args)
          ;; functions like `set-fields!` should propagate the :incomplete? metadata of their
          ;; first argument.
          write-incomplete? (-> args first meta :incomplete?)
          ;; functions like `where` are incomplete if they have an :only arg.
          read-incomplete? (boolean (extract-congo-argument args :only))
          incomplete? (if (nil? write-incomplete?)
                        read-incomplete?
                        write-incomplete?)]
      (throw-if-not (seq? results) "expected seq")
      (map
        (fn [result]
          (when result
            (if (map? result)
              (vary-meta result assoc :incomplete? incomplete?)
              result)))
        results))))

(defn wrap-debug
  "Wrap f to add default output values for the result of f"
  [f]
  (fn [& args]
    (println "coming in:\n" args)
    (let [results (apply f args)]
      (println "coming out:\n" results)
      results)))

(extend-protocol congo-coerce/ConvertibleToMongo
  DateTime
  (clojure->mongo [^DateTime dt]
    (.toDate dt)))

(def utc DateTimeZone/UTC)
(assert utc)

(extend-protocol congo-coerce/ConvertibleFromMongo
  java.util.Date
  (mongo->clojure [^java.util.Date d keywordize]
    (DateTime. d utc)))

(declare coerce)

(defn coerce-map [m {:keys [keywords strings string-keys] :as options}]
  (let [subopts (dissoc options :keywords :string-keys)
        kvps (for [[k v] m]
               (let [k (if string-keys k (keyword k))]
                 (cond
                   (contains? keywords k) [k (keyword v)]
                   (contains? strings k) [k (coerce v (assoc subopts :string-keys true))]
                   :else [k (coerce v subopts)])))]
    (with-meta (into {} kvps) (meta m))))

(defn coerce [obj options]
  (cond
    (instance? java.util.List obj) (mapv #(coerce % options) obj)
    (instance? com.mongodb.DBObject obj)
      (coerce-map (for [k (.keySet ^com.mongodb.DBObject obj)]
                    [k (.get obj k)])
                  options)
    (instance? java.util.Map obj)
      (coerce-map obj options)
    (instance? java.util.Date obj) (DateTime. obj utc)
    :else obj))

(defn wrap-translations
  "If the result of f contains keys which are in keywords, convert them to keywords"
  [f options]
  (fn [& args]
    (binding [*translations* (assoc *translations* [:mongo :clojure] #(coerce % options))]
      (apply f args))))

(defn wrap-validate
  "If validators is true, run validate!"
  [f validators]
  (if validators
    (fn [& args]
      (mv/validate! validators (last args))
      (apply f args))
    f))

(defn wrap-input-defaults
  "Wrap f to add default output values for the result of f"
  [f defaults]
  (if defaults
    (fn [& args]
      (let [args (apply-to-last
                  (fn [val] (apply-defaults defaults val nil)) args)]
        (apply f args)))
    f))

(defn coerce-id
  [id]
  (cond
   (nil? id) (throwf "Expected id, got nil")
   (ref? id) (coerce-id @id)
   (instance? String id) (do (throw-if (= id "") "Got empty string (\"\"), expected id")
                             (congo/object-id id))
   (instance? org.bson.types.ObjectId id) id
   (:_id id)  (:_id id)
   :else (throwf "Expected id, got %s" id)))

(defn intern-fn
  "intern the function in :ns under the name :name"
  [fn {:keys [ns name doc arglists]}]
  (ns-unmap ns (symbol name))
  (intern ns (with-meta (symbol name)
               {:doc doc
                :arglists arglists}) fn))

(defn dot-keyword-to-unnested-vector
  "Takes a dot-notated keyword and splits it into a vector of keywords"
  [k]
  (map keyword (clojure.string/split (name k) #"[\.]")))

(defn convert-dotmap-to-nested
  "Converts a map with dot-notated keys to a nested map"
  [m]
  (reduce (fn [m [k v]] (assoc-in m (dot-keyword-to-unnested-vector k) v)) {} m))

(defn is-dot-notated?
  "Checks if a keyword is in dot-notation"
  [k]
  (boolean (re-find (re-matcher #"[\.]" (name k)))))

(defn deep-merge-with-like-mongo
  "Merge new fields into an old map the same way MongoDB does.
  Note that although this plays nicely with dot-notated nested fields it 
  does not play nicely with indexes at the moment"
  [old_map new_map strings]
  (if (empty? new_map) 
    old_map
    (let [[k v] (first new_map)]
      (if (is-dot-notated? k)
        (deep-merge-with-like-mongo
         (assoc-in old_map
                   (reduce (fn [acc next-key]
                             ;; If the previous key is in strings, then don't
                             ;; keyword-ify the next key in the chain
                             (conj acc (if (contains? strings (last acc))
                                         next-key
                                         (keyword next-key))))
                           [] (clojure.string/split (name k) #"[\.]"))
                   v)
         (dissoc new_map k)
         strings)
        (deep-merge-with-like-mongo 
         (merge old_map {k v}) 
         (dissoc new_map k)
         strings)))))

;;; Some functions return single objects, some return lists. We apply all the
;;; functions to each item in the list, because those lists are lazy. So we need
;;; to instead wrap those single objects as lazy lists, apply all operations
;; using `map`s, and then unwrap the single objects just before returning them.
(defn wrap-wrap-single-object
  [f returns-list]
  (fn [& args]
    (let [result (apply f args)]
      (when-not returns-list (throw-if (seq? result) "didnt expect seq"))
      (if returns-list ;; then it already is a list and we dont wrap it
        result
        (list result)))))

(defn wrap-unwrap-single-object
  [f returns-list]
  (fn [& args]
    (let [results (apply f args)]
      (throw-if-not (seq? results) "expected seq")
      (if returns-list ;; then we a list is wanted, so don't unwrap it
        results
        (first results)))))

(defn str-take [n str]
  (.substring str 0 (min n (count str))))

(defn log-message [x & {:keys [sensitive]}]
  (let [msg (str x)
        msg (if sensitive
              (str/replace msg sensitive "%FILTERED%")
              msg)]
    (str-take 150 msg)))

(defn- milliseconds-since
  [previous-nanotime]
  (Math/round ^Double (double (/ (- (System/nanoTime) previous-nanotime) 1e6))))

(defn wrap-profile
  [f time-in-millis ns name]
  (fn [& args]
    (if-not time-in-millis
      (apply f args)
      (let [start (System/nanoTime)
            result (apply f args)
            msecs (milliseconds-since start)]
        (when (>= msecs time-in-millis)
          (let [sensitive (extract-congo-argument args :sensitive)
                msg (log-message args :sensitive sensitive)]
            (println (format "slow query (%dms): (%s/%s %s)" msecs ns name msg))))
      result))))

(defn get-hooks [desired-phase model-hooks fn-hooks]
  (->> fn-hooks
       (map (fn [[crud [& phases]]]
              (get-in model-hooks [crud desired-phase])))
       (filter identity)))

(defn some-hooks
  "Takes a seq of hooks and returns a function that is the composition of those
  hooks. Adds a nil-guard to each of the hooks, stops executing hooks if passed
  nil or when a hook in the chain returns nil.
  Returns nil if called with nil or if any of the hooks returns nil."
  [hooks]
  (let [guard-nil-fn (fn [hook] (fn [x] (when x (hook x))))]
    (apply comp (map guard-nil-fn hooks))))

(defn call-pre-hooks [hooks row]
  ((some-hooks hooks) row))

(defn call-post-hooks-singular [hooks row]
  ((some-hooks hooks) row))

(defn call-post-hooks-plural [hooks rows]
  (map #(call-post-hooks-singular hooks %) rows))

(defn call-post-hooks [hooks returns-list rows]
  (if returns-list
    (call-post-hooks-plural hooks rows)
    (call-post-hooks-singular hooks rows)))

(defn wrap-hooks [f returns-list model-hooks fn-hooks]
  "Calls the appropriate hooks. model-hooks is the hooks defined in the defmodel. fn-hooks is the hooks on the fn definition."
  (let [pre-hooks (get-hooks :pre model-hooks fn-hooks)
        post-hooks (get-hooks :post model-hooks fn-hooks)]
    (fn [& args]
      (if (or (empty? args) (empty? pre-hooks))
        (call-post-hooks post-hooks returns-list (apply f args))
        (let [[row & opts] args]
          (->> row
               (call-pre-hooks pre-hooks)
               (#(apply f % opts))
               (call-post-hooks post-hooks returns-list)))))))

(defn wrap-fn-middleware
  [f middleware]
  (if (nil? middleware)
    f
    (let [handler (middleware f)]
      (assert (fn? handler) "middleware should return a function")
        handler)))

(defn add-functions
  "Takes a list of hashes which define functions, wraps those functions
  according to their specification, and interns those functions in the target
  namespace"
  [ns function-defs model-hooks fn-middleware]
  (doseq [fdef function-defs]
    (let [{:keys [name fn
                  doc arglists
                  input-ref output-ref
                  input-defaults output-defaults
                  input-transients
                  validate-input
                  keywords
                  strings
                  hooks
                  profile
                  returns-list]
           :or {input-ref false output-ref false
                input-defaults nil output-defaults nil
                input-transients nil
                keywords nil
                profile nil
                hooks []
                validate-input false
                returns-list false}}
          fdef]
      (throw-if (and input-ref output-ref returns-list) "Function expecting the ref to be updated can't use lists")
      (-> fn
          (wrap-profile profile ns name)
          (wrap-translations {:keywords keywords :strings strings})
          (wrap-hooks returns-list (dissoc model-hooks :ref) hooks)
          (wrap-wrap-single-object returns-list)
          (wrap-transients input-transients)
          ;; always run before transient so that you can be required and transient
          (wrap-validate validate-input)
          (wrap-input-defaults input-defaults)
          (wrap-output-defaults output-defaults)
          (wrap-output-incomplete?)
          (wrap-refs input-ref output-ref)
          (wrap-unwrap-single-object returns-list)
          (wrap-hooks returns-list (select-keys model-hooks [:ref]) (when output-ref
                                                                      {:ref [:post]}))
          (wrap-fn-middleware fn-middleware)
          (intern-fn {:ns ns :name name :doc doc :arglists arglists})))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generate the function templates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-row-functions [collection row-validators field-defs defaults transients use-refs keywords strings profile-reads profile-writes]
  (let [validators (into row-validators (col-validators field-defs))

        valid? {:fn (fn [row] (mv/valid? validators row))
                :input-ref use-refs
                :name "valid?"}

        validate!-fn (fn [row] (mv/validate! validators row))
        validate! {:fn validate!-fn
                   :input-ref use-refs
                   :output-ref use-refs
                   :name "validate!"}

        ;;; given a single key, return the object with that key
        find-by-id {:fn (fn [id & options]
                          (when id
                            (apply congo/fetch-by-id collection (coerce-id id) options)))
                    :output-defaults defaults
                    :arglists '([id])
                    :output-ref use-refs
                    :input-ref use-refs
                    :keywords keywords
                    :strings strings
                    :name "find-by-id"
                    :hooks {:load [:post]}
                    :profile profile-reads}

        ;;; given a list of keys, return the objects with those keys
        find-by-ids {:fn (fn [ids & options]
                           (apply congo/fetch-by-ids collection (eager-map coerce-id ids) options))
                     :doc "returns a seq of rows. Options are kw arguments that congo understands, such as :limit and :sort"
                     :arglists '([ids & options])
                     :output-defaults defaults
                     :output-ref use-refs
                     :keywords keywords
                     :strings strings
                     :returns-list true
                     :name "find-by-ids"
                     :hooks {:load [:post]}
                     :profile profile-reads}

        ;;; given conditions, find objects which match
        where {:fn (fn [& options]
                     (let [[cond & options] options
                           cond (or cond {})]
                       (apply congo/fetch collection :where cond options)))
               :doc "returns a seq of rows that match the where clause. 'where' is a congo :where map, options are kw arguments that congo understands, such as :limit and :sort"
               :arglists '([where & options])
               :output-ref use-refs
               :keywords keywords
               :strings strings
               :returns-list true
               :output-defaults defaults
               :name "where"
               :hooks {:load [:post]}
               :profile profile-reads}

        find-count {:fn (fn [& options]
                          (apply congo/fetch-count collection options))
                    :doc "returns the number of rows, to be used with options like :where"
                    :arglists '([& options])
                    :name "find-count"
                    :profile profile-reads}

        ;;; given conditions, find the first object which matches
        find-one {:fn (fn [& options]
                        (apply congo/fetch-one collection options))
                  :doc "returns the first row. options are kw arguments that congo understands, such as :limit, :where and :sort"
                  :arglists '([& options])
                  :output-ref use-refs
                  :keywords keywords
                  :strings strings
                  :output-defaults defaults
                  :name "find-one"
                  :hooks {:load [:post]}
                  :profile profile-reads}

        all {:fn (fn [& options] (apply congo/fetch collection options))
             :doc "returns all rows. 'options' are kw arguments that congo understands, such as :limit, :where and :sort"
             :arglists '([& options])
             :output-ref use-refs
             :keywords keywords
             :strings strings
             :returns-list true
             :output-defaults defaults
             :name "all"
             :hooks {:load [:post]}
             :profile profile-reads}

        nu-fn identity
        nu {:fn nu-fn
            :input-defaults defaults
            :output-ref use-refs
            :input-transients transients
            :validate-input validators
            :keywords keywords
            :strings strings
            :name "nu"}

        create! {:fn (fn [val]
                       (congo/insert! collection (merge {:_id (ObjectId.)} (nu-fn val))))
                 :doc "Takes a map, inserts and returns a new row."
                 :arglists '([row])
                 :input-defaults defaults
                 :output-ref use-refs
                 :input-transients transients
                 :validate-input validators
                 :keywords keywords
                 :strings strings
                 :name "create!"
                 :hooks {:create [:pre]
                         :update [:pre]
                         :load [:post]}
                 :profile profile-writes}

        ;;; raw find-and-modify
        find-and-modify! {:fn (fn [where update & options]
                                (apply congo/fetch-and-modify collection where update options))
                          :doc "basic mongo findAndModify for this collection."
                          :arglists '([where update & options])
                          :input-ref false
                          :output-ref use-refs
                          :keywords keywords
                          :strings strings
                          :returns-list false
                          :output-defaults defaults
                          :name "find-and-modify!"
                          :hooks {:load [:post]
                                  :update [:pre :post]}
                          :profile profile-writes}

        ;; TODO: only the fields being set should be validated
        set-fields! {:fn (fn [old new-fields]
                           (let [new (assert! (congo/fetch-and-modify collection
                                                                      {:_id (coerce-id old)}
                                                                      {:$set new-fields}
                                                                      :return-new? true
                                                                      :only (keys new-fields)
                                                                      :upsert? false)
                                              "Expected result, got nil")]
                             (deep-merge-with-like-mongo old new-fields strings)))

                     :doc "new-fields is a map. mongo atomically $sets the fields in new-field, without disturbing other fields on the row that may have been changed in another thread/process"
                     :arglists '([row new-field])
                     :input-transients transients
                     :input-ref use-refs
                     :output-ref use-refs
                     :keywords keywords
                     :strings strings
                     ;; validate-input validators
                     :name "set-fields!"
                     :hooks {:update [:pre :post]
                             :load [:post]}
                     :profile profile-writes}

        unset-fields! {:fn (fn [old removed-fields]
                             (let [field-map (zipmap removed-fields (repeat 1))
                                   new (assert! (congo/fetch-and-modify collection
                                                                        {:_id (coerce-id old)}
                                                                        {:$unset field-map}
                                                                        :return-new? false
                                                                        :upsert? false)
                                                "Expected result, got nil")]
                               (apply dissoc old removed-fields)))

                       :doc "removed-fields is a seq. mongo atomically $unsets the fields in removed-field, without disturbing other fields on the row that may have been changed in another thread/process"
                       :arglists '([row removed-fields])
                       :input-transients transients
                       :input-ref use-refs
                       :output-ref use-refs
                       :keywords keywords
                       :strings strings
                       ;; validate-input validators
                       :name "unset-fields!"
                       :hooks {:update [:pre :post]
                               :load [:post]}
                       :profile profile-writes}

        push! {:fn (fn [old field value]
                     (let [new (assert! (congo/fetch-and-modify collection
                                                                {:_id (coerce-id old)}
                                                                {:$push {field value}}
                                                                :return-new? true
                                                                :only [field]
                                                                :upsert? false)
                                        "Expected result, got nil")]
                       (assoc old field (field new))))

               :doc "pushes a new value on to the mongo array in field"
               :arglists '([row field value])
               :input-transients transients ;; TODO: this makes no sense
               :input-ref use-refs
               :output-ref use-refs
               :hook :update
               :hooks {:update [:pre :post]
                       :load [:post]}
               :keywords keywords
               :strings strings
               :name "push!"
               :profile profile-writes}

        add-to-set! {:fn (fn [old field value]
                           (let [new (assert! (congo/fetch-and-modify collection
                                                                      {:_id (coerce-id old)}
                                                                      {:$addToSet {field value}}
                                                                      :return-new? true
                                                                      :only [field]
                                                                      :upsert? false)
                                              "Expected result, got nil")]
                             (assoc old field (field new))))
                     :doc "pushes a new value on to the mongo array in field"
                     :arglists '([row field value])
                     :input-transients transients ;; TODO: this makes no sense
                     :input-ref use-refs
                     :output-ref use-refs
                     :hook :update
                     :hooks {:update [:pre :post]
                             :load [:post]}
                     :keywords keywords
                     :strings strings
                     :name "add-to-set!"
                     :profile profile-writes}

        pull! {:fn (fn [old field value]
                     (let [new (assert! (congo/fetch-and-modify collection
                                                                {:_id (coerce-id old)}
                                                                {:$pull {field value}}
                                                                :return-new? true
                                                                :only [field]
                                                                :upsert? false)
                                        "Expected result, got nil")]
                       (assoc old field (field new))))
               :doc "pulls a value from the mongo array in field"
               :arglists '([row field value])
               :input-transients transients ;; TODO: this makes no sense
               :input-ref use-refs
               :output-ref use-refs
               :hook :update
               :keywords keywords
               :strings strings
               :hooks {:update [:pre :post]
                       :load [:post]}
               :name "pull!"
               :profile profile-writes}

        replace!-fn (fn [id new-obj]
                      (congo/update! collection {:_id (coerce-id id)} new-obj :upsert false)
                      (-> new-obj
                          (congo-coerce/coerce [:clojure :mongo])
                          (congo-coerce/coerce [:mongo :clojure])))

        ;; TODO: replace! should always be validated
        replace! {:fn replace!-fn
                  :doc "replaces the row with id id with new-row. Potential race conditions if this row has been updated in another thread/process"
                  :arglists '([id new-row])
                  :input-transients transients
                  :input-ref use-refs
                  :output-ref use-refs
                  :keywords keywords
                  :strings strings
                  :hook :update
                  ;;: validate-input validators
                  :name "replace!"
                  :hooks {:update [:pre :post]
                          :load [:post]}
                  :profile profile-writes}

        ;; TODO: save! should always be validated
        save! {:fn (fn [current] (replace!-fn current current))
               :doc "save row in the DB. Row must contain an :_id field"
               :arglists '([row])
               :input-transients transients
               :input-ref use-refs
               :output-ref use-refs
               :hook :update
               :keywords keywords
               :strings strings
               :validate-input validators
               :name "save!"
               :hooks {:update [:pre :post]
                       :load [:post]}
               :profile profile-writes}]

    [valid? validate!
     find-by-id find-by-ids
     where find-one
     all
     nu create!
     find-count
     find-and-modify!
     set-fields! unset-fields!
     replace! save!
     push! pull! add-to-set!]))

(defn create-col-function [collection field defaults transients use-refs keywords strings profile-reads profile-writes]
  (let [{:keys [findable default validators name required transient foreign]} field

        find-one-by-X-fn (fn [val & options]
                           (apply congo/fetch-one collection :where {(keyword name) val} options))
        find-by-X-fn (fn [val & options]
                       (apply congo/fetch collection :where {(keyword name) val} options))
        find-by-X-template {:output-ref use-refs
                            :output-defaults defaults
                            :keywords keywords
                            :strings strings
                            :profile profile-reads
                            :hooks {:load [:post]}}
        
        find-by-X (assoc find-by-X-template
                    :fn find-by-X-fn
                    :returns-list true
                    :name (format "find-by-%s" (clojure.core/name name)))
        
        find-one-by-X (assoc find-by-X-template
                        :fn find-one-by-X-fn
                        :returns-list false
                        :name (format "find-one-by-%s" (clojure.core/name name)))
        
        find-by-X! (assoc find-by-X-template
                     :fn (fn [val & options]
                           (-> (apply find-by-X-fn val options)
                               (throw-if-not "Couldn't find row with %s=%s on collection %s" name val collection)))
                     :returns-list true
                     :name (format "find-by-%s!" (clojure.core/name name)))

        find-one-by-X! (assoc find-by-X-template
                         :fn (fn [val & options]
                               (-> (apply find-one-by-X-fn val options)
                                   (throw-if-not "Couldn't find row with %s=%s on collection %s" name val collection)))
                         :returns-list false
                         :name (format "find-one-by-%s!" (clojure.core/name name)))]
    (when findable
      ;; check that there is an index on this field
      [find-by-X find-by-X! find-one-by-X find-one-by-X!])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Controls the show
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn canonicalize-field-defs
  "Validate field definitions"
  [{:keys [name required findable default validators keyword transient]
    :as args}]
  (throw-if-not name "Missing name field in definition: %s" args)
  (let [result (merge {:required false
                       :findable false
                       :default nil
                       :keyword nil
                       :strings nil
                       :validator nil
                       :transient false} args)]
    (throw-if-not (= (count result) 8)
                  "Unexpected keys found in %s" args)
    result))

(defn defmodel
  "Define a DB model from its fields"
  [collection & {:keys [validators fields use-refs profile-reads profile-writes hooks fn-middleware]
                 :or {validators [] fields [] use-refs false hooks {}}
                 :as attrs}]
  (let [fields (into [] (eager-map canonicalize-field-defs fields))
        defaults (into [] (eager-map (fn [f] [(:name f) (:default f)]) fields))
        transients (into [] (eager-map :name (filter :transient fields)))
        keywords (into #{} (eager-map :name (filter :keyword fields)))
        strings (into #{} (eager-map :name (filter :strings fields)))
        row-templates (create-row-functions collection validators fields defaults transients use-refs keywords strings profile-reads profile-writes)
        col-templates (apply concat (for [f fields] (create-col-function collection f defaults transients use-refs keywords strings profile-reads profile-writes)))]
    (add-functions *ns* (into [] (concat col-templates row-templates)) hooks fn-middleware)))


(defn defapi [&args])
