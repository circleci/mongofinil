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
       (map :validators)
       (filter identity)
       (concat validators)))


(defn create-row-functions [namespace collection validators]
  (with-ns/with-ns namespace
    (declare valid? validate! find-by-id new create!)

    (let [collection# ~collection
          validators# ~validators]
      (defn valid? [row#] (mv/valid? validators# row#))
      (defn validate! [row#] (mv/validate! validators# row#))
      (defn find-by-id [id#] (congo/fetch-by-id collection# id#))
      (defn nu [& {:as args#}] (~'validate! args#) args#)
      (defn create! [& {:as args#}] (->> args# ~'nu (congo/insert! collection#)))
      )))

(defn canonicalize-field
  "Validate field definitions"
  [{:keys [name required findable default validators]
    :as args}]
  (throw-if-not name)
  (merge {:required false :findable false :default nil :validators []} args))

(defn format-fn-name [pattern name]
  (symbol (format pattern name)))


;; TODO default, and required
(defn create-col-function [namespace collection field]
  (with-ns/with-ns namespace
    (let [{:keys [findable default validators name required]} field]
      (when findable
        (defn (format-fn-name "get-by-%s" name#)
          [val] (congo/fetch-one ~collection
                                 :where {(keyword name#) val}))

        (defn (format-fn-name "get-by-%s!" name#) [val]
          (assert!
           (~(format-fn-name "get-by-%s" name#) val)))))))



(defn create-col-functions [namespace collection fields]
  (doall (map #(create-col-function namespace collection %) fields)))

(defmacro defmodel
  "Define a DB model from its fields"
  [collection & {:keys [validators fields]
                 :or {validators [] fields []}}]
  (let [fields (into [] (map canonicalize-field fields))
;        vs (all-validators validators fields)
        ]
    `(do

       ;;       (create-row-functions vs)
       (create-col-functions ~*ns* ~collection ~fields)
       )))


(defn defapi [&args])



;; Everything defaults to nil
;; (create-mapping build
;;   (db-field :vcs_url)
;;   (db-field :vcs_revision)
;;   (db-field :build_num)
;;   (db-field :start_time)
;;   (db-field :stop_time)
;;   (db-field :infrastructure_fail)
;;   (db-field :timedout)
;;   (db-field :why)

;;   (db-field :failed)
;;   (db-field :parents)
;;   (db-field :subject)
;;   (db-field :body)
;;   (db-field :branch)

;;   (db-field :committer_name)
;;   (db-field :committer_email)
;;   (db-field :committer_date)
;;   (db-field :author_email)
;;   (db-field :author_name)
;;   (db-field :author_date)

;;  has_many :action_logs, :inverse_of => :thebuild

;;  belongs_to :user ;  # who started the build, can be null
;;  belongs_to :project
;; )