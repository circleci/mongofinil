(ns mongofinil.test-indexes
  (:use midje.sweet)
  (:require [somnium.congomongo :as congo])
  (:require [mongofinil.core :as core])
  (:require [mongofinil.testing-utils :as utils])
  (:import org.bson.types.ObjectId))

(utils/setup-test-db)
(utils/setup-midje)

(fact "with missing index, we get a warning"
  (with-out-str (core/defmodel :xs :fields [{:name :x :findable true}]))
  =>
  #"Missing index for :x in :xs")

(fact "with present index, we get no warning"
  (congo/add-index! :xs [:x])
  (with-out-str (core/defmodel :xs :fields [{:name :x :findable true}]))
  => "")