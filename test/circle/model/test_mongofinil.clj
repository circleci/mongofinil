(ns circle.model.test-mongofinil1
  (:use midje.sweet)
  (:require [clojure.contrib.with-ns :as with-ns])
  (:require [somnium.congomongo :as congo])
  (:require [circle.init])
  (:require [circle.test-utils :as test])
  (:require [circle.model.mongofinil :as mongofinil]))

(test/test-ns-setup)


(mongofinil/defmodel :xs
  :fields [;; simple
           {:name :x :findable true}
           {:name :y :findable false}
           {:name :z}
           {:name :w :findable true}

           ;; default
           {:name :dx :default 5}
           {:name :dy :default (fn [b] 6)}
           {:name :dz :default (fn [b] (-> b :x))}
           ])

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


(fact "apply-defaults works"
  (mongofinil/apply-defaults {:x 5 :y (fn [v] 6) :z 10} {:z 9}) => (contains {:x 5 :y 6 :z 9}))



(fact "incorrectly named attributes are caught"
  (eval `(mongofinil/defmodel :ys :fields [{:a :b}])) => throws
  (eval `(mongofinil/defmodel :ys :fields [{:name :b :unexpected-field :y}])) => throws)


(fact "default works on creation"
  (create! :x 7) => (contains {:dx 5 :dy 6 :dz 7})
  (nu :x 7) => (contains {:dx 5 :dy 6 :dz 7}))

(fact "default works on loading"
  (congo/insert! :xs {:x 22})
  (find-by-x! 22) => (contains {:dx 5 :dy 6 :dz 22})
  (find-by-x 22) => (contains {:dx 5 :dy 6 :dz 22}))


(fact "dissoc causes things not to be saved to the DB")

(fact "dissoc doesnt stop things being loaded from the DB")
                                        ; check dissoc on each function


(fact "id key is present after creation")

(fact "dissoc still have the values in the table")

(fact "joda time roundtrip when fetching from the DB")

(fact "refresh function exists and works (refreshes from DB")

(fact "update function exists and works (updates DB)")