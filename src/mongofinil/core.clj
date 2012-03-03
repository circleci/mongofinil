(ns mongofinil.core
  "A Mongoid-like library that lets you focus on the important stuff"

  (:require [somnium.congomongo :as congo])

  (:use [mongofinil.helpers :only (assert! throw-if-not throw-if coerce-object-id)])
  (:require [mongofinil.validation :as mv])
  (:require [mongofinil.validation-helpers :as mvh]))


(defn to-id
  "Given a string, ObjectId or hash, return the appropriate ObjectId"
  [id]
  (cond
   (instance? String id) (congo/object-id id)
   (instance? org.bson.types.ObjectId id) id
   :else (:_id id)))

(defn reintern
  "unmap and remap the function"
  [ns sym func]
  (ns-unmap ns sym)
  (intern ns sym func))

(defn col-validators
  "Returns a vector of validators from field definitions"
  [fields]
  (->> fields
       (map :validator)
       (filter identity)

       ;; validator for the require attribute
       (into [(mvh/require-keys (map :name (filter :required fields)))])))

(defn apply-defaults
  "Given an object, fill in the missing fields with their defaults"
  [defaults values]
  (-> (into {} (for [[k v] defaults]
                 (cond
                  (contains? k values) [k (-> values k)]
                  (fn? v) [k (v values)]
                  (nil? v) nil
                  :else [k v])))
      (merge values)))

(defn apply-dissocs
  "Given an object, fill in the missing fields with their defaults"
  [dissocs values]
  (apply dissoc values dissocs))

(defn create-row-functions [ns collection row-validators field-defs defaults dissocs]
  (let [validators (into row-validators (col-validators field-defs))]

    (reintern ns 'valid?
              (fn [row]
                (mv/valid? validators row)))

    (reintern ns 'validate!
              (fn [row]
                (mv/validate! validators row)))

    (reintern ns 'find
              (fn [id]
                (let [id (to-id id)]
                  (->> (congo/fetch-by-id collection id)
                       (apply-defaults defaults)))))

    (reintern ns 'find-one
              (fn [& args]
                (let [result (apply congo/fetch-one collection args)]
                  (when result
                    (apply-defaults defaults result)))))

    (reintern ns 'nu
              (fn [& {:as vals}]
                (let [validate! (ns-resolve ns 'validate!)
                      vals (apply-defaults defaults vals)
                      vals (apply-dissocs dissocs vals)]
                  (validate! vals)
                  vals)))

    (reintern ns 'create!
              (fn [& vals]
                (let [nu (ns-resolve ns 'nu)
                      vals (apply nu vals)]
                  (congo/insert! collection vals))))

    (reintern ns 'instance-count
              (fn []
                (congo/fetch-count collection)))


    (reintern ns 'set-fields!
              (fn [obj & {:as vals}]
                (let [id (to-id obj)
                      args (apply-dissocs dissocs vals)]
                  (congo/fetch-and-modify collection {:_id id} {:$set args}))))))

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

(defn i [ns format-str name f]
  (let [n (symbol (format format-str (clojure.core/name name)))]
    (reintern ns n f)))

;; Note that we need all the defaults here, not just the default for this field
(defn create-col-function [ns collection field defaults dissocs]
  (let [{:keys [findable default validators name required dissoc]} field]
    (when findable

      (i ns "find-by-%s" name
         (fn [val & args]
           (let [result (apply congo/fetch-one collection :where {(keyword name) val} args)]
             (when result
               (apply-defaults defaults result)))))

      (i ns "find-by-%s!" name
         (fn [val & args]
           (let [result (apply congo/fetch-one collection :where {(keyword name) val} args)]
             (throw-if-not result "Couldn't find row with %s=%s on collection %s" name val collection)
             (apply-defaults defaults result)))))))


(defn defmodel
  "Define a DB model from its fields"
  [collection & {:keys [validators fields]
                    :or {validators [] fields []}}]
  (let [fields (into [] (map canonicalize-field-defs fields))
        defaults (into {} (map (fn [f] [(:name f) (:default f)]) fields))
        dissocs (into [] (map :name (filter :dissoc fields)))]
    (create-row-functions *ns* collection validators fields defaults dissocs)
    (doseq [f fields] (create-col-function *ns* collection f defaults dissocs))))


(defn defapi [&args])
