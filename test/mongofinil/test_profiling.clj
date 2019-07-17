(ns mongofinil.test-profiling
  (:require [midje.sweet :refer (fact)]

            [mongofinil.core :as core]
            [mongofinil.testing-utils :as utils]))

(utils/setup-test-db)
(utils/setup-midje)

(core/defmodel :xs :fields [] :profile-reads 0 :profile-writes 5)

(fact "slow operations warn"
  (let [warning (with-out-str (create! {:val "abc" :x (into [] (range 50000))}))]
    warning => #"slow query \(\d+ms\):"
    warning => #"mongofinil.test-profiling/create!"))

(fact "fast operations do not warn"
  (with-out-str (create! {:val 1 :x 1}))
  => "")

(fact "sensitive data can be filtered"
  (let [warning (with-out-str (find-one :where {:val "abc"} :sensitive "abc"))]
    (assert (not (re-find #"abc" warning)))))
