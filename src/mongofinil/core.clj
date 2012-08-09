(ns mongofinil.core
  "A Mongoid-like library that lets you focus on the important stuff"

  (:require [somnium.congomongo :as congo]
            [somnium.congomongo.coerce :as congo-coerce]
            [mongofinil.validation :as mv]
            [mongofinil.validation-helpers :as mvh]
            [clj-time.core :as time]
            [clojure.contrib.string :as string2])

  (:use [mongofinil.helpers :only (assert! throw-if-not throw-if ref? throwf eager-map inspect)])
  (:import org.bson.types.ObjectId))


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

(defn apply-defaults
  [defaults row only]
  (if (empty? defaults)
    row
    (do
      (throw-if-not (map? row) "Expected a hash, got %s" row)
      (reduce (fn [r d]
                (let [[k v] d
                      only (when only (set only))]
                  (if (and only (not (contains? only k))) ; dont apply
                                        ; defaults if not in the 'only set
                    r
                    (assoc r k
                           (cond
                            (contains? r k) (get r k)
                            (fn? v) (v r)
                            :else v)))))
              row defaults))))

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

(defn wrap-convert-keywords
  "If the result of f contains keys which are in keywords, convert them to keywords"
  [f keywords]
  (if keywords
    (fn [& args]
      (let [results (apply f args)]
        (throw-if-not (seq? results) "expected seq")
        (map
         (fn [result]
           (if (map? result)
             (do
               (-> (for [[k v] result]
                     (if (contains? keywords k) [k (keyword v)]
                         [k v]))
                   (#(into {} %))))
             result))
         results)))
    f))

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

(defn wrap-profile
  [f time-in-millis ns name]
  (fn [& args]
    (if-not time-in-millis
      (apply f args)
      (let [start (time/now)
            result (apply f args)
            stop (time/now)
            msecs (time/in-msecs (time/interval start stop))]
        (when (> msecs time-in-millis)
          (println (format "slow query (%dms): (%s/%s %s)" msecs ns name (string2/take 150 (str args)))))
        result))))

(defn get-hooks [desired-phase model-hooks fn-hooks]
  (->> fn-hooks
       (map (fn [[crud [& phases]]]
              (get-in model-hooks [crud desired-phase])))
       (filter identity)))

(defn call-pre-hooks [hooks rows]
  (reduce (fn [result hook]
            (map hook result)) rows hooks))

(defn call-post-hooks-singular [hooks row]
  (reduce (fn [result hook]
            (hook result)) row hooks))

(defn call-post-hooks-plural [hooks rows]
  (reduce (fn [result hook]
            (map hook result)) rows hooks))

(defn call-post-hooks [hooks returns-list rows]
  (let [f (if returns-list
            call-post-hooks-plural
            call-post-hooks-singular)]
    (f hooks rows)))

(defn wrap-hooks [f returns-list model-hooks fn-hooks]
  "Calls the appropriate hooks. model-hooks is the hooks defined in the defmodel. fn-hooks is the hooks on the fn definition."
  (let [pre-hooks (get-hooks :pre model-hooks fn-hooks)
        post-hooks (get-hooks :post model-hooks fn-hooks)]
    (fn [& rows]
      (->> rows
           (call-pre-hooks pre-hooks)
           (apply f)
           (call-post-hooks post-hooks returns-list)))))

(defn add-functions
  "Takes a list of hashes which define functions, wraps those functions
  according to their specification, and interns those functions in the target
  namespace"
  [ns function-defs model-hooks]
  (doseq [fdef function-defs]
    (let [{:keys [name fn
                  doc arglists
                  input-ref output-ref
                  input-defaults output-defaults
                  input-transients
                  validate-input
                  keywords
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
          (wrap-hooks returns-list model-hooks hooks)
          (wrap-wrap-single-object returns-list)
          (wrap-transients input-transients)
          ;; always run before transient so that you can be required and transient
          (wrap-validate validate-input)
          (wrap-input-defaults input-defaults)
          (wrap-output-defaults output-defaults)
          (wrap-convert-keywords keywords)
          (wrap-refs input-ref output-ref)
          (wrap-unwrap-single-object returns-list)
          (intern-fn {:ns ns :name name :doc doc :arglists arglists})))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generate the function templates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-row-functions [collection row-validators field-defs defaults transients use-refs keywords profile-reads profile-writes]
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
                  :output-defaults defaults
                  :name "find-one"
                  :hooks {:load [:post]}
                  :profile profile-reads}

        all {:fn (fn [& options] (apply congo/fetch collection options))
             :doc "returns all rows. 'options' are kw arguments that congo understands, such as :limit, :where and :sort"
             :arglists '([& options])
             :output-ref use-refs
             :keywords keywords
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
                 :name "create!"
                 :hooks {:create [:pre]
                         :load [:post]}
                 :profile profile-writes}

        ;; TODO: only the fields being set should be validated
        set-fields! {:fn (fn [old new-fields]
                           (let [new (assert! (congo/fetch-and-modify collection
                                                                      {:_id (coerce-id old)}
                                                                      {:$set new-fields}
                                                                      :return-new? true
                                                                      :upsert? false)
                                              "Expected result, got nil")
                                 new-fields (select-keys new (keys new-fields))]
                             (merge old new-fields)))

                     :doc "new-fields is a map. mongo atomically $sets the fields in new-field, without disturbing other fields on the row that may have been changed in another thread/process"
                     :arglists '([row new-field])
                     :input-transients transients
                     :input-ref use-refs
                     :output-ref use-refs
                     :keywords keywords
                     :hooks {:update [:pre :post]}
                     ;; validate-input validators
                     :name "set-fields!"
                     :profile profile-writes}

        unset-fields! {:fn (fn [old removed-fields]
                             (let [field-map (zipmap removed-fields (repeat 1))
                                   new (assert! (congo/fetch-and-modify collection
                                                                        {:_id (coerce-id old)}
                                                                        {:$unset field-map}
                                                                        :return-new? true
                                                                        :upsert? false)
                                                "Expected result, got nil")]
                               (apply dissoc old removed-fields)))

                       :doc "removed-fields is a seq. mongo atomically $unsets the fields in removed-field, without disturbing other fields on the row that may have been changed in another thread/process"
                       :arglists '([row removed-fields])
                       :input-transients transients
                       :input-ref use-refs
                       :output-ref use-refs
                       :keywords keywords
                       :hook {:update [:pre :post]}
                       ;; validate-input validators
                       :name "unset-fields!"
                       :profile profile-writes}

        push! {:fn (fn [old field value]
                     (let [new (assert! (congo/fetch-and-modify collection
                                                                {:_id (coerce-id old)}
                                                                {:$push {field value}}
                                                                :return-new? true
                                                                :upsert? false)
                                        "Expected result, got nil")]
                       (assoc old field (field new))))

               :doc "pushes a new value on to the mongo array in field"
               :arglists '([row field value])
               :input-transients transients ;; TODO: this makes no sense
               :input-ref use-refs
               :output-ref use-refs
               :hook :update
               :keywords keywords
               :name "push!"
               :hooks {:update [:pre :post]}
               :profile profile-writes}

        add-to-set! {:fn (fn [old field value]
                           (let [new (assert! (congo/fetch-and-modify collection
                                                                      {:_id (coerce-id old)}
                                                                      {:$addToSet {field value}}
                                                                      :return-new? true
                                                                      :upsert? false)
                                              "Expected result, got nil")]
                             (assoc old field (field new))))
                     :doc "pushes a new value on to the mongo array in field"
                     :arglists '([row field value])
                     :input-transients transients ;; TODO: this makes no sense
                     :input-ref use-refs
                     :output-ref use-refs
                     :hook :update
                     :keywords keywords
                     :name "add-to-set!"
                     :hooks {:update [:pre :post]}
                     :profile profile-writes}

        pull! {:fn (fn [old field value]
                     (let [new (assert! (congo/fetch-and-modify collection
                                                                {:_id (coerce-id old)}
                                                                {:$pull {field value}}
                                                                :return-new? true
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
               :name "pull!"
               :hooks {:update [:pre :post]}
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
                  :hook :update
                  ;;: validate-input validators
                  :name "replace!"
                  :hooks {:update [:pre :post]}
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
               :validate-input validators
               :name "save!"
               :hooks {:update [:pre :post]}
               :profile profile-writes}]

    [valid? validate!
     find-by-id find-by-ids
     where find-one
     all
     nu create!
     find-count
     set-fields! unset-fields!
     replace! save!
     push! pull! add-to-set!]))

(defn create-col-function [collection field defaults transients use-refs keywords profile-reads profile-writes]
  (let [{:keys [findable default validators name required transient foreign]} field

        find-one-by-X-fn (fn [val & options]
                           (apply congo/fetch-one collection :where {(keyword name) val} options))
        find-by-X-fn (fn [val & options]
                       (apply congo/fetch collection :where {(keyword name) val} options))

        find-by-X {:fn find-by-X-fn
                   :output-ref use-refs
                   :output-defaults defaults
                   :returns-list true
                   :keywords keywords
                   :profile profile-reads
                   :name (format "find-by-%s" (clojure.core/name name))}

        find-one-by-X {:fn find-one-by-X-fn
                       :output-ref use-refs
                       :output-defaults defaults
                       :keywords keywords
                       :profile profile-reads
                       :name (format "find-one-by-%s" (clojure.core/name name))}

        find-by-X! {:fn (fn [val & options]
                          (-> (apply find-by-X-fn val options)
                              (throw-if-not "Couldn't find row with %s=%s on collection %s" name val collection)))
                    :output-defaults defaults
                    :returns-list true
                    :output-ref use-refs
                    :keywords keywords
                    :profile profile-reads
                    :name (format "find-by-%s!" (clojure.core/name name))}

        find-one-by-X! {:fn (fn [val & options]
                              (-> (apply find-one-by-X-fn val options)
                                  (throw-if-not "Couldn't find row with %s=%s on collection %s" name val collection)))
                        :output-defaults defaults
                        :output-ref use-refs
                        :keywords keywords
                        :profile profile-reads
                        :name (format "find-one-by-%s!" (clojure.core/name name))}]
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
                       :validator nil
                       :transient false} args)]
    (throw-if-not (= (count result) 7)
                  "Unexpected keys found in %s" args)
    result))

(defn defmodel
  "Define a DB model from its fields"
  [collection & {:keys [validators fields use-refs profile-reads profile-writes hooks]
                 :or {validators [] fields [] use-refs false hooks {}}
                 :as attrs}]
  (let [fields (into [] (eager-map canonicalize-field-defs fields))
        defaults (into [] (eager-map (fn [f] [(:name f) (:default f)]) fields))
        transients (into [] (eager-map :name (filter :transient fields)))
        keywords (into #{} (eager-map :name (filter :keyword fields)))
        row-templates (create-row-functions collection validators fields defaults transients use-refs keywords profile-reads profile-writes)
        col-templates (apply concat (for [f fields] (create-col-function collection f defaults transients use-refs keywords profile-reads profile-writes)))]
    (add-functions *ns* (into [] (concat col-templates row-templates)) hooks)))


(defn defapi [&args])