(ns mongofinil.test-core-required
  (:require [midje.sweet :refer (contains fact future-fact throws)]

            [mongofinil.core :as core]
            [mongofinil.testing-utils :as utils]))

(utils/setup-test-db)
(utils/setup-midje)

(core/defmodel :xs
  :fields [;; simple
           {:name :z}
           {:name :w :findable true}

           ;; required
           {:name :rx :required true}

           ;; requires and transient
           {:name :rt :required true :transient true}
           ])

;; This is broken, because all declared columns are now always in the map defaulting to nil.
(future-fact "required works"
  (create! {:x 5 :rt 5}) => (throws Exception)
  (create! {:x 6 :rx 7 :rt 7}) => (contains {:x 6 :rx 7 :rt 7})
  (find-count) => 1)

(fact "required and transient is legit"
  (create! {:x 5 :rt 5 :rx 17}) => (contains {:x 5 :rt 5 :rx 17})
  (find-count) => 1
  (find-one) =not=> (contains {:rt 5})
  (find-one) => (contains {:x 5 :rx 17}))
