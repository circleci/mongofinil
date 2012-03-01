(ns circle.model.mongofinil
  "A mongoid-like library that lets you focus on the important stuff"
  (:require [clojure.contrib.with-ns :as with-ns]))


(defn all-validators [validators fields]
  (->> fields
       (map :validators)
       (filter identity)
       (concat validators)))


(defmacro create-row-functions [collection validators]
  `(do
     (require '[circle.util.model-validation :as mv])
     (require '[circle.util.model-validation-helpers :as mvh])
     (require '[somnium.congomongo :as congo])
     (declare ~'valid? ~'validate! ~'find-by-id ~'new ~'create!)

     (let [collection# ~collection
           validators# ~validators]
       (defn ~'valid? [row#] (mv/valid? validators# row#))
       (defn ~'validate! [row#] (mv/validate! validators# row#))
       (defn ~'find-by-id [id#] (congo/fetch-by-id collection# id#))
       (defn ~'nu [& {:as args#}] (~'validate! args#) args#)
       (defn ~'create! [& {:as args#}] (->> args# ~'nu (congo/insert! collection#)))
       )))

;; (defn canonicalize-field
;;   (fn [& {:keys [name required findable]
;;          :or {:required false :findable false}
;;          :as args}]
;;     (throw-if-not name)
;;     args))

;; (defn create-col-function [collection field]
;;   (let [{:keys name required findable default} field
;;         i (fn [pattern func] (intern *ns* (symbol (format pattern name)) func))]

;;     ;; TODO default, and required
;;     (when findable
;;       (i :get-by-%s (fn [val] (congo/fetch-one collection
;;                                               :where {(keyword name) val})))

;;       (i :get-by-%s! (fn [val] ((assert!
;;                                 (congo/fetch-one collection
;;                                                  :where {(keyword name) val})))))))
;;    )

;; (defn create-col-functions [fields]
;;   (doseq [f fields] (create-col-functions f)))

;; (defn defmodel
;;   "Define a DB model from its fields"
;;   [collection & {:keys [validators fields]
;;                  :or {:validators [] :fields []}}]

;;   (let [fields (map canonicalize-field fields)
;;         vs (all-validators validators fields)]
;;     (create-row-functions vs)
;;     (create-col-functions fields)))


(defn defapi)



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