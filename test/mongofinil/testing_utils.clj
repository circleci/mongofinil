(ns mongofinil.testing-utils
  (:use midje.sweet)
  (:require [somnium.congomongo :as congo]))

(def test-db
  {:db :mongofinil_test_db
   :host "127.0.0.1"
   :port 27017})

(defn congo-connect [{:keys [db host port]}]
  (congo/make-connection db :host host :port port))

(defn setup-test-db
  "Initializes the mongodb connection"
  []
  (congo/set-connection! (congo-connect test-db)))

(defn clear-test-db []
  (doseq [c (filter #(not= "system.indexes" %) (congo/collections))]
    (congo/drop-coll! c)))


(defmacro setup-midje
  "Defines a midje (background) such that the test DB is cleared
  between runs, and all clojure DB connections go through the test DB"
  [& initializers]
  `(background (before :facts (do (clear-test-db)
                                  ~@initializers))))
