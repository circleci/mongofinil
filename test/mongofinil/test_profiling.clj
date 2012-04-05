(ns mongofinil.test-profiling
  (:require [somnium.congomongo :as congo]
            [mongofinil.core :as core]
            [mongofinil.testing-utils :as utils]
            [clj-time.core :as time])
  (:use midje.sweet))

(utils/setup-test-db)
(utils/setup-midje)

(core/defmodel :xs :fields [] :profile-reads 1 :profile-writes 1)

(fact "slow operations warn"
  (let [warning (with-out-str (create! {:val 1 :x (into [] (range 5000))}))]
    warning => #"slow query \(\d+ms\):"
    warning => #"mongofinil.test-profiling/create!"))

(fact "fast operations do not warn"
  (with-out-str (create! {:val 1 :x 1}))
  => "")