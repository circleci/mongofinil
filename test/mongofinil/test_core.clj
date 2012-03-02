(ns mongofinil.test-core
  (:use midje.sweet)
  (:require [somnium.congomongo :as congo])
  (:require [clj-time.core :as time])
  (:require [clj-time.coerce :as coerce-time])
  (:require [mongofinil.core :as core])
  (:require [mongofinil.test-utils :as utils]))

(utils/setup-test-sb)
(utisl/setup-midje)

(core/defmodel :xs
  :fields [;; simple
           {:name :x :findable true}
           {:name :y :findable false}
           {:name :z}
           {:name :w :findable true}

           ;; default
           {:name :dx :default 5}
           {:name :dy :default (fn [b] 6)}
           {:name :dz :default (fn [b] (-> b :x))}

           {:name :disx :dissoc true}
           ])

(fact "row findable functions are created and work"
  (let [obj1 (create! :x 1 :y 2 :z 3 :w 4)
        obj2 (create! :x 2 :y 3 :z 4 :w 5)]

    ;; create worked and added an id
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
  (core/apply-defaults {:x 5 :y (fn [v] 6) :z 10} {:z 9}) => (contains {:x 5 :y 6 :z 9}))



(fact "incorrectly named attributes are caught"
  (eval `(core/defmodel :ys :fields [{:a :b}])) => throws
  (eval `(core/defmodel :ys :fields [{:name :b :unexpected-field :y}])) => throws)


(fact "default works on creation"
  (create! :x 7) => (contains {:dx 5 :dy 6 :dz 7})
  (nu :x 7) => (contains {:dx 5 :dy 6 :dz 7}))

(fact "default works on loading"
  (congo/insert! :xs {:x 22})
  (find-by-x! 22) => (contains {:dx 5 :dy 6 :dz 22})
  (find-by-x 22) => (contains {:dx 5 :dy 6 :dz 22}))


(fact "dissoc causes things not to be saved to the DB"
  ;; TODO: We expect the dissoc to be applied on creation, and so the resulting
  ;; object would not have the value. Maybe that's wrong, but it's fine for now.
  ;; It's probably wrong for update though
  (create! :disx 5 :x 12) =not=> (contains {:disx 5})

  (find-by-x 12) =not=> (contains {:disx 5}))


(fact "dissoc doesnt stop things being loaded from the DB"
  (congo/insert! :xs {:disx 55 :x 55})
  (find-by-x 55) => (contains {:disx 55}))


(fact "canonicalize functions work"
  (let [now (time/now)
        date (coerce-time/to-date now)]
    (core/canonicalize-from-db {:x date}) => {:x now}
    (core/canonicalize-from-db {:x 5}) => {:x 5}
    (core/canonicalize-value-from-db date) => now

    (core/canonicalize-to-db {:x now}) => {:x date}
    (core/canonicalize-to-db {:x 5}) => {:x 5}
    (core/canonicalize-value-to-db now) => date))



(fact "joda time roundtrip when fetching from the DB"
   (let [now (time/now)]
     (create! :x now) => truthy
     (find-by-x now) => (contains {:x now})))



(fact "refresh function exists and works (refreshes from DB")

(fact "update function exists and works (updates DB)")

(fact "check validations work"); valid? and validate!
