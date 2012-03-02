(ns circle.model.mongofinil
  "A Mongoid-like library that lets you focus on the important stuff"
  (:require [clojure.contrib.with-ns :as with-ns])
  (:require [clj-time.coerce :as coerce-time])
  (:require [somnium.congomongo :as congo])

  (:use [circle.util.except :only (assert! throw-if-not throw-if)])
  (:use [circle.util.mongo :only (coerce-object-id)])
  (:require [circle.util.model-validation :as mv])
  (:require [circle.util.model-validation-helpers :as mvh]))


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

(defn canonicalize-output
  [hash]
  (doseq [[k v] hash]
    [k
     (cond
      (instance? java.util.Date v) (coerce-time/to-date-time v)
      :else v)]))

(defn all-validators [validators fields]
  (->> fields
       (map :validator)
       (into [(mvh/require-keys (map :name (filter :required fields)))])
       (filter identity)
       (concat validators)
       (into [])))

(defn create-row-functions [ns collection validators defaults]

  (reintern ns 'valid? (fn [row]
                         (mv/valid? validators row)))

  (reintern ns 'validate! (fn [row]
                            (mv/validate! validators row)))

  (reintern ns 'find (fn [id]
                       (let [id (to-id id)]
                         (canonicalize-output
                          (congo/fetch-by-id collection)))))

  (reintern ns 'find-one (fn [id & args]
                           (canonicalize-output
                            (apply congo/fetch-one collection args))))

  (reintern ns 'nu
            (fn [& {:as vals}] (let [vals (merge defaults vals)
                                    validate! (ns-resolve ns 'validate!)]
                                (validate! vals)
                                vals)))
  (reintern ns 'create! (fn [& args]
                          (let [nu (ns-resolve ns 'nu)
                                vals (apply nu args)]
                            (congo/insert! collection vals))))

  (reintern ns 'set-fields! (fn [first & {:as args}]
                              (let [id (to-id first)]
                                (congo/fetch-and-modify collection {:_id id} {:$set args})))))

(defn row-defaults
  "Returns a map of default values, including nil for values with no default"
  [field-defs]
  (into {} (map (fn [f] [(:default f) (:name f)]) field-defs)))

(defn canonicalize-field
  "Validate field definitions"
  [{:keys [name required findable default validators]
    :as args}]
  (throw-if-not name)
  (let [result (merge {:required false :findable false :default nil :validators []} args)]
    (throw-if-not (= (count result) 5))
    result))

(defn i [ns format-str name f]
  (let [n (symbol (format format-str (clojure.core/name name)))]
    (reintern ns n f)))

;; TODO default, and required
(defn create-col-function [ns collection field]
  (let [{:keys [findable default validators name required]} field]
    (when findable
      (i ns "find-by-%s" name
         (fn [val & args]
           (apply congo/fetch-one collection :where {(keyword name) val} args)))
      (i ns "find-by-%s!" name
         (fn [val & args]
           (throw-if-not
            (apply congo/fetch-one collection :where {(keyword name) val} args)
            "Couldn't find row with %s=%s on collection %s" name val collection))))))


(defn create-col-functions [namespace collection fields]
  (doall (map #(create-col-function namespace collection %) fields)))

(defmacro defmodel
  "Define a DB model from its fields"
  [collection & {:keys [validators fields]
                 :or {validators [] fields []}}]
  (let [fields (into [] (map canonicalize-field fields))
        defaults (row-defaults fields)]
    `(do
       (let [fields# ~fields]
         (create-row-functions *ns* ~collection (all-validators ~validators fields#) ~defaults)
         (create-col-functions *ns* ~collection fields#))
       )))


(defn defapi [&args])
