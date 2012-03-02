(ns circle.model.test-mongofinil1
  (:use midje.sweet)
  (:require [clojure.contrib.with-ns :as with-ns])
  (:require [circle.init])
  (:require [circle.test-utils :as test])
  (:require [circle.model.mongofinil :as mongofinil]))

(test/test-ns-setup)


(mongofinil/defmodel :xs
  :fields [;; simple
           {:name :z}
           {:name :w :findable true}

           ;; required
           {:name :rx :required true}])

(fact "required works"
  (create! :x 5) => throws
  (create! :x 6 :rx 7) => (contains {:x 6 :rx 7}))