(ns mongofinil.core
  "A Mongoid-like library that lets you focus on the important stuff"

  (:require [somnium.congomongo :as congo])
  (:require [somnium.congomongo.coerce :as congo-coerce])

  (:use [mongofinil.helpers :only (assert! throw-if-not throw-if ref? throwf)])
  (:require [mongofinil.validation :as mv])
  (:require [mongofinil.validation-helpers :as mvh])
  (:import org.bson.types.ObjectId))


(defn col-validators
  "Returns a vector of validators from field definitions"
  [fields]
  (->> fields
       (map :validator)
       (filter identity)

       ;; validator for the require attribute
       (into [(mvh/require-keys (map :name (filter :required fields)))])))

(defn apply-to-last [f args]
  (throw-if-not (seq? args) "Expected seq, got %s" args)
  (let [skipped (butlast args)
        val (last args)
        val (f val)
        args (concat skipped [val])]
    args))

(defn wrap-dissocs
  "Take the last arguent and strips the transient attributes from it. Apply the
  DB operation, and then readdd the transient attributes to its result"
  [f dissocs]
  (if dissocs
    (fn [& args]
      (throw-if-not (-> args last map?) "Expecting map, got %s" (last args))
      (let [val (last args)
            rest (butlast args)
            transient (select-keys val dissocs)
            dissoced (apply dissoc val dissocs)
            args (concat rest [dissoced])
            result (apply f args)
            readded (when result
                      (merge transient result))]
        readded))
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
                                result (apply f derefed)]
                            (dosync (ref-set first result))
                            first))
   output? (fn [& args] (ref (apply f args)))
   input? (fn [& args]
            (throw-if-not (-> args first ref?) "Expecting ref, got %s" (first args))
            (apply f (concat [@(first args)] (rest args))))
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
      (let [result (apply f args)]
        (when result
          (apply-defaults defaults result))))
    f))

(defn wrap-convert-keywords
  "If the result of f contains keys which are in keywords, convert them to keywords"
  [f keywords]
  (if keywords
    (fn [& args]
      (let [result (apply f args)]
        (if (map? result)
          (do
            (-> (for [[k v] result]
                  (if (contains? keywords k) [k (keyword v)]
                      [k v]))
                (#(into {} %))))
          result)))
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
   (instance? String id) (congo/object-id id)
   (instance? org.bson.types.ObjectId id) id
   (:_id id)  (:_id id)
   :else (throwf "Expected id, got %s" id)))

(defn intern-fn
  "intern the function in :ns under the name :name"
  [fn ns name]
  (ns-unmap ns (symbol name))
  (intern ns (symbol name) fn))

(defn add-functions
  "Takes a list of hashes which define functions, wraps those functions
  according to their specification, and interns those functions in the target
  namespace"
  [ns function-defs]
  (doseq [fdef function-defs]
    (let [{:keys [name fn
                  input-ref output-ref
                  input-defaults output-defaults
                  input-dissocs
                  validate-input
                  keywords]
           :or {input-ref false output-ref false
                input-defaults nil output-defaults nil
                input-dissocs nil
                keywords nil
                validate-input false}}
          fdef]
      (-> fn
          (wrap-dissocs input-dissocs)
          ;; always run before dissoc so that you can be required and transient
          (wrap-validate validate-input)
          (wrap-input-defaults input-defaults)
          (wrap-output-defaults output-defaults)
          (wrap-convert-keywords keywords)
          (wrap-refs input-ref output-ref)
          (intern-fn ns name)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generate the function templates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-row-functions [collection row-validators field-defs defaults dissocs use-refs keywords]
  (let [validators (into row-validators (col-validators field-defs))

        valid? {:fn (fn [row] (mv/valid? validators row))
                :input-ref use-refs
                :name "valid?"}

        validate!-fn (fn [row] (mv/validate! validators row))
        validate! {:fn validate!-fn
                   :input-ref use-refs
                   :name "validate!"}

        find {:fn (fn [id] (congo/fetch-by-id collection (coerce-id id)))
              :output-defaults defaults
              :output-ref use-refs
              :keywords keywords
              :name "find"}

        find-one {:fn (fn [& options] (apply congo/fetch-one collection options))
                  :output-defaults defaults
                  :output-ref use-refs
                  :keywords keywords
                  :name "find-one"}

        nu-fn identity
        nu {:fn nu-fn
            :input-defaults defaults
            :output-ref use-refs
            :input-dissocs dissocs
            :validate-input validators
            :keywords keywords
            :name "nu"}

        create! {:fn (fn [val]
                       (congo/insert! collection (merge {:_id (ObjectId.)} (nu-fn val))))
                 :input-defaults defaults
                 :output-ref use-refs
                 :input-dissocs dissocs
                 :validate-input validators
                 :keywords keywords
                 :name "create!"}

        instance-count {:fn (fn [] (congo/fetch-count collection))
                        :name "instance-count"}

        set-fields! {:fn (fn [old new-fields]
                           (congo/fetch-and-modify collection
                                                   {:_id (:_id old)}
                                                   {:$set new-fields}
                                                   :return-new? true
                                                   :upsert? false))
                     :input-dissocs dissocs
                     :input-ref use-refs
                     :output-ref use-refs
                     :keywords keywords
                     :name "set-fields!"}

        update! {:fn (fn [old new]
                       (congo/update! collection {:_id (:_id old)} new :upsert false)
                       (-> new
                           (congo-coerce/coerce [:clojure :mongo])
                           (congo-coerce/coerce [:mongo :clojure])))
                 :input-dissocs dissocs
                 :input-ref use-refs
                 :output-ref use-refs
                 :keywords keywords
                 :name "update!"}]

    [valid? validate! find find-one nu create! instance-count set-fields! update!]))

(defn create-col-function [collection field defaults dissocs use-refs keywords]
  (let [{:keys [findable default validators name required dissoc]} field
        find-by-X-fn (fn [val & args]
                       (apply congo/fetch-one collection :where {(keyword name) val} args))
        find-by-X {:fn find-by-X-fn
                   :output-ref use-refs
                   :output-defaults defaults
                   :keywords keywords
                   :name (format "find-by-%s" (clojure.core/name name))}

        find-by-X! {:fn (fn [val & args]
                           (-> (apply find-by-X-fn val args)
                               (throw-if-not "Couldn't find row with %s=%s on collection %s" name val collection)))
                    :output-defaults defaults
                    :output-ref use-refs
                    :keywords keywords
                    :name (format "find-by-%s!" (clojure.core/name name))}]
    (if findable
      [find-by-X find-by-X!]
      [])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Controls the show
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn canonicalize-field-defs
  "Validate field definitions"
  [{:keys [name required findable default validators keyword dissoc]
    :as args}]
  (throw-if-not name "Missing name field in definition: %s" args)
  (let [result (merge {:required false
                       :findable false
                       :default nil
                       :keyword nil
                       :validators []
                       :dissoc false} args)]
    (throw-if-not (= (count result) 7)
                  "Unexpected keys found in %s" args)
    result))

(defn defmodel
  "Define a DB model from its fields"
  [collection & {:keys [validators fields use-refs]
                 :or {validators [] fields [] use-refs false}
                 :as attrs}]
  (let [fields (into [] (map canonicalize-field-defs fields))
        defaults (into [] (map (fn [f] [(:name f) (:default f)]) fields))
        dissocs (into [] (map :name (filter :dissoc fields)))
        keywords (into #{} (map :name (filter :keyword fields)))
        row-templates (create-row-functions collection validators fields defaults dissocs use-refs keywords)
        col-templates (apply concat (for [f fields] (create-col-function collection f defaults dissocs use-refs keywords)))]
    (add-functions *ns* (into [] (concat col-templates row-templates)))))


(defn defapi [&args])