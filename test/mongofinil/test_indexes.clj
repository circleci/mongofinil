(ns mongofinil.test-indexes
  (:use midje.sweet)
  (:require [somnium.congomongo :as congo])
  (:require [mongofinil.core :as core])
  (:require [mongofinil.testing-utils :as utils])
  (:import org.bson.types.ObjectId))

(utils/setup-test-db)
(utils/setup-midje)

(core/defmodel :xs :fields [{:name :x :findable true}])

(fact "with missing index, we get a warning"
  (with-out-str (warn-indexes!)) => #"Missing index for :x in :xs")

(fact "with present index, we get no warning"
  (congo/add-index! :xs [:x])
  (with-out-str (warn-indexes!)) => "")

(fact "with unique index, we get no warning"
  (congo/add-index! :xs [:x] :unique true)
  (with-out-str (warn-indexes!)) => "")

(fact "with reverse index, we get no warning"
  (congo/add-index! :xs [[:x -1]])
  (with-out-str (warn-indexes!)) => "")