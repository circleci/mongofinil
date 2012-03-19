(ns mongofinil.core
  "A Mongoid-like library that lets you focus on the important stuff"

  (:require [somnium.congomongo :as congo])
  (:require [somnium.congomongo.coerce :as congo-coerce])

  (:use [mongofinil.helpers :only (assert! throw-if-not throw-if ref? throwf eager-map)])
  (:require [mongofinil.validation :as mv])
  (:require [mongofinil.validation-helpers :as mvh])
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
                                _ (throw-if-not (ref? first) "Expecting a ref, got %s" first)
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
               (map ref results)))
   input? (fn [& args]
            (throw-if-not (-> args first ref?) "Expecting ref, got %s" (first args))
            (apply f @(first args) (rest args)))
   :else f))

(defn apply-defaults
  [defaults row]
  (if (empty? defaults)
    row
    (do
      (throw-if-not (map? row) "Expected a hash, got %s" row)
      (reduce (fn [r d]
                (let [[k v] d]
                  (if v
                    (assoc r k (cond
                                (contains? r k) (get r k)
                                (fn? v) (v r)
                                :else v))
                    r)))
              row defaults))))

(defn wrap-output-defaults
  "Wrap f to add default output values for the result of f"
  [f defaults]
  (if defaults
    (fn [& args]
      (let [results (apply f args)]
        (throw-if-not (seq? results) "expected seq")
        (map
         (fn [result]
           (when result
             (apply-defaults defaults result)))
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
                  (fn [val] (apply-defaults defaults val)) args)]
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

(defn add-functions
  "Takes a list of hashes which define functions, wraps those functions
  according to their specification, and interns those functions in the target
  namespace"
  [ns function-defs]
  (doseq [fdef function-defs]
    (let [{:keys [name fn
                  doc arglists
                  input-ref output-ref
                  input-defaults output-defaults
                  input-transients
                  validate-input
                  keywords
                  returns-list]
           :or {input-ref false output-ref false
                input-defaults nil output-defaults nil
                input-transients nil
                keywords nil
                validate-input false
                returns-list false}}
          fdef]
      (throw-if (and input-ref output-ref returns-list) "Function expecting the ref to be updated can't use lists")
      (-> fn
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

(defn create-row-functions [collection row-validators field-defs defaults transients use-refs keywords]
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
        find-by-id {:fn (fn [id] (congo/fetch-by-id collection (coerce-id id)))
                    :output-defaults defaults
                    :arglists '([id])
                    :output-ref use-refs
                    :keywords keywords
                    :name "find-by-id"}

        ;;; given a list of keys, return the objects with those keys
        find-by-ids {:fn (fn [ids & options]
                           (apply congo/fetch-by-ids collection (eager-map coerce-id ids) options))
                     :doc "returns a seq of rows. Options are kw arguments that congo understands, such as :limit and :sort"
                     :arglists '([ids & options])
                     :output-defaults defaults
                     :output-ref use-refs
                     :keywords keywords
                     :returns-list true
                     :name "find-by-ids"}

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
               :name "where"}

        ;;; given conditions, find the first object which matches
        find-one {:fn (fn [& options]
                        (let [[cond & options] options
                              cond (or cond {})]
                          (apply congo/fetch-one collection :where cond options)))
                  :doc "returns the first row that matches the where clause. 'where' is a congo :where map, options are kw arguments that congo understands, such as :limit and :sort"
                  :arglists '([where & options])
                  :output-ref use-refs
                  :keywords keywords
                  :output-defaults defaults
                  :name "find-one"}

        all {:fn (fn [& options] (apply congo/fetch collection options))
             :doc "returns all rows. 'where' is a congo :where map, options are kw arguments that congo understands, such as :limit and :sort"
             :arglists '([& options])
             :output-ref use-refs
             :keywords keywords
             :returns-list true
             :output-defaults defaults
             :name "all"}

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
                 :name "create!"}

        find-count {:fn (fn [& options]
                          (let [[cond & options] options
                                cond (or cond {})]
                            (apply congo/fetch-count collection :where cond options)))
                    :doc "returns the number of rows that match the query. a :where map may be provided as the first option"
                    :arglists '([& options])
                    :name "find-count"}

        ;; TODO: only the fields being set should be validated
        set-fields! {:fn (fn [old new-fields]
                           (assert! (congo/fetch-and-modify collection
                                                            {:_id (coerce-id old)}
                                                            {:$set new-fields}
                                                            :return-new? true
                                                            :upsert? false)
                                    "Expected result, got nil"))
                     :doc "new-fields is a map. mongo atomically $sets the fields in new-field, without disturbing other fields on the row that may have been changed in another thread/process"
                     :arglists '([row new-field])
                     :input-transients transients
                     :input-ref use-refs
                     :output-ref use-refs
                     :keywords keywords
                     ;; validate-input validators
                     :name "set-fields!"}

        push! {:fn (fn [old field value]
                     (assert! (congo/fetch-and-modify collection
                                                      {:_id (coerce-id old)}
                                                      {:$push {field value}}
                                                      :return-new? true
                                                      :upsert? false)
                              "Expected result, got nil"))
               :doc "pushes a new value on to the mongo array in field"
               :arglists '([row field value])
               :input-transients transients ;; TODO: this makes no sense
               :output-ref use-refs
               :keywords keywords
               :name "push!"}

        pull! {:fn (fn [old field value]
                     (assert! (congo/fetch-and-modify collection
                                                      {:_id (coerce-id old)}
                                                      {:$pull {field value}}
                                                      :return-new? true
                                                      :upsert? false)
                              "Expected result, got nil"))
               :doc "pulls a value from the mongo array in field"
               :arglists '([row field value])
               :input-transients transients ;; TODO: this makes no sense
               :output-ref use-refs
               :keywords keywords
               :name "pull!"}

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
                  ;;: validate-input validators
                  :name "replace!"}

        ;; TODO: save! should always be validated
        save! {:fn (fn [current] (replace!-fn current current))
               :doc "save row in the DB. Row must contain an :_id field"
               :arglists '([row])
               :input-transients transients
               :input-ref use-refs
               :output-ref use-refs
               :keywords keywords
               :validate-input validators
               :name "save!"}]

    [valid? validate!
     find-by-id find-by-ids
     where find-one
     all
     nu create!
     find-count
     set-fields!
     replace! save!
     push! pull!]))

(defn create-col-function [collection field defaults transients use-refs keywords]
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
                   :name (format "find-by-%s" (clojure.core/name name))}

        find-one-by-X {:fn find-one-by-X-fn
                       :output-ref use-refs
                       :output-defaults defaults
                       :keywords keywords
                       :name (format "find-one-by-%s" (clojure.core/name name))}

        find-by-X! {:fn (fn [val & options]
                          (-> (apply find-by-X-fn val options)
                              (throw-if-not "Couldn't find row with %s=%s on collection %s" name val collection)))
                    :output-defaults defaults
                    :returns-list true
                    :output-ref use-refs
                    :keywords keywords
                    :name (format "find-by-%s!" (clojure.core/name name))}

        find-one-by-X! {:fn (fn [val & options]
                              (-> (apply find-one-by-X-fn val options)
                                  (throw-if-not "Couldn't find row with %s=%s on collection %s" name val collection)))
                        :output-defaults defaults
                        :output-ref use-refs
                        :keywords keywords
                        :name (format "find-one-by-%s!" (clojure.core/name name))}]
    (if findable
      [find-by-X find-by-X! find-one-by-X find-one-by-X!]
      [])))


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
  [collection & {:keys [validators fields use-refs]
                 :or {validators [] fields [] use-refs false}
                 :as attrs}]
  (let [fields (into [] (eager-map canonicalize-field-defs fields))
        defaults (into [] (eager-map (fn [f] [(:name f) (:default f)]) fields))
        transients (into [] (eager-map :name (filter :transient fields)))
        keywords (into #{} (eager-map :name (filter :keyword fields)))
        row-templates (create-row-functions collection validators fields defaults transients use-refs keywords)
        col-templates (apply concat (for [f fields] (create-col-function collection f defaults transients use-refs keywords)))]
    (add-functions *ns* (into [] (concat col-templates row-templates)))))


(defn defapi [&args])