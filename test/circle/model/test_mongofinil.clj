(ns circle.model.test-mongofinil
  (:use midje.sweet)
  (:require [circle.init])
  (:require [circle.test-utils :as test])
  (:require [circle.model.mongofinil :as mongofinil]))

(mongofinil/defmodel :xs
  :fields [{:name :x :findable true}
           {:name :y :findable false}
           {:name :z}
           {:name :w :findable true}])

(test/test-ns-setup)

(fact "row findable functions are created and work"
  (let [obj1 (create! :x 1 :y 2 :z 3 :w 4)
        obj2 (create! :x 2 :y 3 :z 4 :w 5)]

    ;; create worked
    obj1 =not=> nil
    obj2 =not=> nil
    (-> obj1 :_id) =not=> nil
    (-> obj2 :_id) =not=> nil
    (dissoc obj1 :_id) => (contains {:x 1 :y 2 :z 3 :w 4})
    (dissoc obj2 :_id) => (contains {:x 2 :y 3 :z 4 :w 5})

    ;; success
    (find-by-x 1) => obj1
    (find-by-x! 1) => obj1
    (find-by-w 4) => obj1
    (find-by-w! 4) => obj1
    (find-by-x 2) => obj2
    (find-by-x! 2) => obj2
    (find-by-w 5) => obj2
    (find-by-w! 5) => obj2

    ;; failure
    (resolve 'find-by-y) => nil
    (resolve 'find-by-y!) => nil

    ;; empty
    (find-by-x 3) => nil
    (find-by-w 2) => nil
    (find-by-x! 2) => throws
    (find-by-w! 2) => throws))



(fact "incorrectly named attributes are caught")






(fact "col functions are created")

(fact "required throws errors on creation")
(fact "required does nothing when fields are provided")

(fact "default creates defaults on loading")
(fact "default creates defaults on creation")
; check defaults on each function

(fact "default creates defaults during find-by-X")

(fact "function defaults are called")

(fact "dissoc causes things not to be saved to the DB")

(fact "dissoc doesnt stop things being loaded from the DB")
; check dissoc on each function

(fact "id key is present after creation")

(fact "dissoc still have the values in the table")

(fact "joda time roundtrip when fetching from the DB")

(fact "refresh function exists and works (refreshes from DB")

(fact "update function exists and works (updates DB)")