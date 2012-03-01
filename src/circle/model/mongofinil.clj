(ns circle.model.mongofinil
  "A Mongoid-like library that lets you focus on the important stuff
  (:use [clojure.contrib.except :only (throw-if-not)])"
  (:use [circle.util.except :only (assert! throw-if-not)])
  (:require [clojure.contrib.with-ns :as with-ns])
  (:require [circle.util.model-validation :as mv])
  (:require [circle.util.model-validation-helpers :as mvh])
  (:require [somnium.congomongo :as congo]))


(defn all-validators [validators fields]
  (->> fields
       (map :validator)
       (into [])
       (filter identity)
       (concat validators)
       ))

(defn create-row-functions [ns collection validators]
  (intern ns 'valid? (fn [row] (mv/valid? validators row)))
  (intern ns 'validate! (fn [row] (mv/validate! validators row)))
  (intern ns 'find-by-id (fn [id] (congo/fetch-by-id collection id)))
  (intern ns 'nu (fn [& {:as args}] ((ns-resolve ns 'validate!) args) args))

  (intern ns 'create! (fn [& {:as args}] (->> args
                                             (ns-resolve ns 'nu)
                                             (congo/insert! collection))))
  )

(defn canonicalize-field
  "Validate field definitions"
  [{:keys [name required findable default validators]
    :as args}]
  (throw-if-not name)
  (merge {:required false :findable false :default nil :validators []} args))

(defn i [ns format-str name f]
  (let [n (symbol (format format-str (clojure.core/name name)))]
    (ns-unmap ns n)
    (intern ns n f)))

;; TODO default, and required
(defn create-col-function [ns collection field]
  (let [{:keys [findable default validators name required]} field]
    (when findable
      (i ns "get-by-%s" name
         (fn [val]
           (congo/fetch-one collection :where {(keyword name) val})))
      (i ns "get-by-%s!" name
         (fn [val]
           (throw-if-not  (congo/fetch-one collection :where {(keyword name) val})
                         "Couldn't find row with %s=%s on collection %s" name val collection))))))


(defn create-col-functions [namespace collection fields]
  (doall (map #(create-col-function namespace collection %) fields)))

(defmacro defmodel
  "Define a DB model from its fields"
  [collection & {:keys [validators fields]
                 :or {validators [] fields []}}]
  (let [fields (into [] (map canonicalize-field fields))
        vs (all-validators validators fields)
        ]
    `(do
       (create-row-functions *ns* ~collection ~vs)
       (create-col-functions *ns* ~collection ~fields))))


(defn defapi [&args])
