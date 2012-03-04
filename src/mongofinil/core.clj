(ns mongofinil.core
  "A Mongoid-like library that lets you focus on the important stuff"

  (:require [somnium.congomongo :as congo])

  (:use [mongofinil.helpers :only (assert! throw-if-not throw-if coerce-object-id)])
  (:require [mongofinil.validation :as mv])
  (:require [mongofinil.validation-helpers :as mvh]))


(defn col-validators
  "Returns a vector of validators from field definitions"
  [fields]
  (->> fields
       (map :validator)
       (filter identity)

       ;; validator for the require attribute
       (into [(mvh/require-keys (map :name (filter :required fields)))])))

(defn apply-to-last [f args]
  (let [skipped (butlast args)
        val (last args)
        val (f val)
        args (concat skipped [val])]
    args))

(defn wrap-dissocs
  "Wrap f to dissoc keys from the arguments when treated as a hash."
  [f dissocs]
  (if dissocs
    (fn [& args]
      (let [dissoced (apply-to-last
                      (fn [val] (apply dissoc val dissocs)) args)]
        (apply f dissoced)))
    f))

(defn wrap-input-ref
  "If use?, take the first input values out of its ref"
  [f use?]
  (if use?
    (fn [id & args] (apply f @id args))
    f))

(defn wrap-output-ref
  "If use?, return a ref of the real result"
  [f use?]
  (if use?
    (fn [& args] (ref (apply f args)))
    f))

(defn apply-defaults
  [defaults vals]
  (if defaults
    (-> (for [[k v] defaults]
          (cond
           (contains? k vals) [k (get vals k)]
           (fn? v) [k (v vals)]
           (nil? v) nil
           :else [k v]))
        (#(into {} %))
        (merge vals))
    vals))

(defn wrap-output-defaults
  "Wrap f to add default output values for the result of f"
  [f defaults]
  (if defaults
    (fn [& args]
      (let [result (apply f args)]
        (when result
          (apply-defaults defaults result))))
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
   (instance? clojure.lang.IDeref id) (coerce-id @id)
   (instance? String id) (congo/object-id id)
   (instance? org.bson.types.ObjectId id) id
   :else (:_id id)))

(defn wrap-coerce-input
  "Wrap f to take the first value, and coerce it to an ObjectId"
  [f use?]
  (if use?
    (fn [id & args]
      (apply f (coerce-id id) args))
    f))

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
                  coerce-id-input]
           :or {input-ref false output-ref false
                input-defaults nil output-defaults nil
                input-dissocs nil
                coerce-id-input false}}
          fdef]
      (-> fn
          ;; instead of using wrap-input-ref, all the current cases are handled
          ;; by the conditional checking for refs in coerce-input. I suspect
          ;; this will change with update, but we haven't implemented that yet
;          (wrap-input-ref input-ref)
          (wrap-coerce-input coerce-id-input)
          (wrap-input-defaults input-defaults)
;         (wrap-dissocs input-dissocs)
          (wrap-output-defaults output-defaults)
          (wrap-output-ref output-ref)
          (intern-fn ns name)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generate the function templates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-row-functions [collection row-validators field-defs defaults dissocs use-refs]
  (let [validators (into row-validators (col-validators field-defs))

        valid? {:fn (fn [row] (mv/valid? validators row))
                :input-ref use-refs
                :name "valid?"}

        validate!-fn (fn [row] (mv/validate! validators row))
        validate! {:fn validate!-fn
                   :input-ref use-refs
                   :name "validate!"}

        find {:fn (fn [id] (congo/fetch-by-id collection id))
              :output-defaults defaults
              :coerce-id-input true
              :output-ref use-refs
              :name "find"}

        find-one {:fn (fn [& options] (apply congo/fetch-one collection options))
                  :output-defaults defaults
                  :output-ref use-refs
                  :name "find-one"}

        nu-fn (fn [val]
                (validate!-fn val)
                val)
        nu {:fn nu-fn
            :input-defaults defaults
            :output-ref use-refs
            :input-dissocs dissocs
            :name "nu"}

        create! {:fn (fn [val] (congo/insert! collection (nu-fn val)))
                :input-defaults defaults
                :output-ref use-refs
                :input-dissocs dissocs
                :name "create!"}

        instance-count {:fn (fn [] (congo/fetch-count collection))
                        :name "instance-count"}

        set-fields! {:fn (fn [id update]
                           (congo/fetch-and-modify collection {:_id id} {:$set update}))
                     :coerce-id-input true
                     :input-dissocs dissocs
                     :input-ref use-refs
                     :output-ref use-refs
                     :name "set-fields!"}

        update! {:fn (fn [id new]
                       (congo/update! collection {:_id id} new))
                 :coerce-input-id true
                 :input-dissocs true
                 :input-ref use-refs
                 :output-ref use-refs
                 :name "update!"}]

    [valid? validate! find find-one nu create! instance-count set-fields! update!]))

(defn create-col-function [collection field defaults dissocs use-refs]
  (let [{:keys [findable default validators name required dissoc]} field
        find-by-X-fn (fn [val & args]
                       (apply congo/fetch-one collection :where {(keyword name) val} args))
        find-by-X {:fn find-by-X-fn
                   :output-ref use-refs
                   :output-defaults defaults
                   :name (format "find-by-%s" (clojure.core/name name))}

        find-by-X! {:fn (fn [val & args]
                           (-> (apply find-by-X-fn val args)
                               (throw-if-not "Couldn't find row with %s=%s on collection %s" name val collection)))
                    :output-defaults defaults
                    :output-ref use-refs
                    :name (format "find-by-%s!" (clojure.core/name name))}]
    (if findable
      [find-by-X find-by-X!]
      [])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Controls the show
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn canonicalize-field-defs
  "Validate field definitions"
  [{:keys [name required findable default validators]
    :as args}]
  (throw-if-not name "Missing name field in definition: %s" args)
  (let [result (merge {:required false
                       :findable false
                       :default nil
                       :validators []
                       :dissoc false} args)]
    (throw-if-not (= (count result) 6)
                  "Unexpected keys found in %s" args)
    result))

(defn defmodel
  "Define a DB model from its fields"
  [collection & {:keys [validators fields use-refs]
                 :or {validators [] fields [] use-refs false}
                 :as attrs}]
  (let [fields (into [] (map canonicalize-field-defs fields))
        defaults (into {} (map (fn [f] [(:name f) (:default f)]) fields))
        dissocs (into [] (map :name (filter :dissoc fields)))
        row-templates (create-row-functions collection validators fields defaults dissocs use-refs)
        col-templates (apply concat (for [f fields] (create-col-function collection f defaults dissocs use-refs)))]
    (add-functions *ns* (into [] (concat col-templates row-templates)))))


(defn defapi [&args])