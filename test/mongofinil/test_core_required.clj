(ns mongofinil.test-core-required
  (:use midje.sweet)
  (:require [mongofinil.core :as core])
  (:require [mongofinil.testing-utils :as utils]))

(utils/setup-test-db)
(utils/setup-midje)

(core/defmodel :xs
  :fields [;; simple
           {:name :z}
           {:name :w :findable true}

           ;; required
           {:name :rx :required true}])

(fact "required works"
  (create! {:x 5}) => throws
  (create! {:x 6 :rx 7}) => (contains {:x 6 :rx 7}))