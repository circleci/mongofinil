(ns mongofinil.test-utils
  (:require [somnium.congomongo :as congo]))

(def test-db
  {:db :mongofinil_test_db
   :host "127.0.0.1"
   :port 27017})

(defn congo-connect [{:keys [db host port username password]}]
  (congo/make-connection db :host host :port port))

(defn setup-test-db
  "Initializes the mongodb connection"
  [& [db]]
  (congo/set-connection! (congo-connect db)))

(defn clear-test-db []
  (doseq [c (filter #(not= "system.indexes" %) (congo/collections))]
    (congo/drop-coll! c)))


(defmacro setup-midje
  "Defines a midje (background) such that the test DB is cleared
  between runs, and all clojure DB connections go through the test DB"
  []
  `(background (before :facts (do (clear-test-db)))))
