(ns mongofinil.test-hooks
  (:require [bond.james :as bond]
            [somnium.congomongo :as congo]
            [mongofinil.core :as core]
            [mongofinil.testing-utils :as utils])
  (:use midje.sweet
        [clojure.core.incubator :only (-?>)])
  (:import org.bson.types.ObjectId))


(utils/setup-test-db)
(utils/setup-midje)

(defn update-hook [row]
  (update-in row [:hook-count] (fnil inc 0)))

(defn pre-update-hook [query]
  (update-in query [:pre-hooks] (fnil inc 0)))

(defn load-hook [row]
  (update-in row [:loaded] (fnil inc 0)))

(core/defmodel :xs
  :fields [{:name :x :findable true}]
  :hooks {:update {:post #'update-hook
                   :pre #'pre-update-hook}
          :load {:post #'load-hook}})

(fact "post hooks are triggered"
  (bond/with-spy [load-hook]
    (let [orig (create! {:x "x"})]
      orig => (contains {:hook-count 1})
      (-> load-hook bond/calls count) => 1))
  (bond/with-spy [load-hook]
    (find-one-by-x "x")
    (-> load-hook bond/calls count) => 1))

(fact "pre hooks are triggered"
  (bond/with-spy [pre-update-hook]
    (let [orig (create! {})]
      orig => (contains {:pre-hooks 1})
      (-> pre-update-hook bond/calls count) => 1
      (let [update (set-fields! orig {:c 1})]
        update => (contains {:pre-hooks 2})
        (-> pre-update-hook bond/calls count) => 2))))

(fact "post hooks aren't called when no rows are returned"
  (bond/with-spy [load-hook]
    (seq (find-one :where {:bogus 987})) => nil
    (-> load-hook bond/calls count) => 0))

(fact "pre hooks aren't called when there is no query"
  (bond/with-spy [pre-update-hook]
    (find-one) => anything
    (-> pre-update-hook bond/calls count) => 0))

(fact "handles options to find-and-modify"
  (bond/with-spy [pre-update-hook]

    (find-and-modify! {:bogus 987} {:$set {:b 2}} :upsert? false)
    (-> (all) count) => 0
    (-> pre-update-hook bond/calls count) => 1

    (find-and-modify! {:bogus 987} {:$set {:b 2}} :upsert? true)
    (-> (all) count) => 1
    (-> pre-update-hook bond/calls count) => 2))
