(ns mongofinil.test-core
  (:use midje.sweet)
  (:require [somnium.congomongo :as congo])
  (:require [mongofinil.core :as core])
  (:require [mongofinil.testing-utils :as utils]))

(utils/setup-test-db)
(utils/setup-midje)

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

           {:name :disx :dissoc true}])

(fact "row findable functions are created and work"
  (let [obj1 (create! {:x 1 :y 2 :z 3 :w 4})
       obj2 (create! {:x 2 :y 3 :z 4 :w 5})]

    obj1 =not=> nil
    obj2 =not=> nil
    (-> obj1 :_id) =not=> nil
    (-> obj2 :_id) =not=> nil
    (dissoc obj1 :_id) => (contains {:x 1 :y 2 :z 3 :w 4})
    (dissoc obj2 :_id) => (contains {:x 2 :y 3 :z 4 :w 5})

    ;; success
    (find-by-x 1) => (contains obj1)
    (find-by-x! 1) => (contains obj1)
    (find-by-w 4) => (contains obj1)
    (find-by-w! 4) => (contains obj1)
    (find-by-x 2) => (contains obj2)
    (find-by-x! 2) => (contains obj2)
    (find-by-w 5) => (contains obj2)
    (find-by-w! 5) => (contains obj2)

    ;; failure
    (resolve 'find-by-y) => nil
    (resolve 'find-by-y!) => nil

    ;; empty
    (find-by-x 3) => nil
    (find-by-w 2) => nil
    (find-by-x! 2) => throws
    (find-by-w! 2) => throws))

(fact "find works"
  (let [obj (create! {:x 5 :y 6})
        id (:_id obj)]
    (find obj) => (contains obj)
    (find id) => (contains obj)
    (find (str id)) => (contains obj)
    (find (congo/object-id (str id))) => (contains obj)))

(fact "find-one works"
  (create! {:x 5 :y 6})
  (find-one) => (contains {:x 5 :y 6})
  (find-one :where {:y 6}) => (contains {:x 5 :y 6})
  (find-one :where {:y 7}) => nil)


(fact "apply-defaults works"
  (core/apply-defaults {:x 5 :y (fn [v] 6) :z 10} {:z 9}) => (contains {:x 5 :y 6 :z 9}))


(fact "default works on creation"
  (create! {:x 7}) => (contains {:dx 5 :dy 6 :dz 7})
  (nu {:x 7}) => (contains {:dx 5 :dy 6 :dz 7}))

(fact "default works on loading"
  (congo/insert! :xs {:x 22})
  (find-by-x! 22) => (contains {:dx 5 :dy 6 :dz 22})
  (find-by-x 22) => (contains {:dx 5 :dy 6 :dz 22}))

(fact "no nil defaults"
  (create! {:x 5}) =not=> (contains {:y anything}))


(fact "dissoc causes things not to be saved to the DB"
  ;; TODO: We expect the dissoc to be applied on creation, and so the resulting
  ;; object would not have the value. Maybe that's wrong, but it's fine for now.
  ;; It's probably wrong for update though
  (create! {:disx 5 :x 12}) =not=> (contains {:disx 5})

  (find-by-x 12) =not=> (contains {:disx 5}))


(fact "ensure set works as planned"
  ;; add and check expected values
  (create! {:a "b" :c "d"})
  (let [old (find-one)]
    (-> old :a) => "b"
    (-> old :c) => "d"

    ;; set and check expected values
    (let [result (set-fields! old {:a "x" :e "f"})
          count (instance-count)
          new (find-one)]
      count => 1
      result => old
      (-> new :a) => "x"
      (-> new :c) => "d"
      (-> new :e) => "f")))

;.;. a ({:x :w, :a :b})
;.;. ({:x :w, :a :b})
;.;. x ({:a :b, :x :w, :dx 5, :dy 6, :dz :w})
;.;.
;.;. [31mFAIL[0m at (NO_SOURCE_FILE:1)
;.;. Actual result did not agree with the checking function.
;.;.         Actual result: {:x "w", :a "b", :_id #<ObjectId 4f52d9973004b386b48d20b2>, :dx 5, :dy 6, :dz "w"}
;.;.     Checking function: (contains {:c "d", :a "B", :x "w"})
;.;.     The checker said this about the reason:
;.;.         Best match found: {:x "w"}.
(fact "update! works"
  (let [x (create! {:a :b :x :w})]
    (update! x {:c :d :a :B}))
  (find-one) => (contains {:c "d" :a "B" :x "w"}))


(future-fact "dissoc doesnt stop things being loaded from the DB"
  (congo/insert! :xs {:disx 55 :x 55})
  (find-by-x 55) => (contains {:disx 55}))

(future-fact "refresh function exists and works (refreshes from DB")

(future-fact "update function exists and works (updates DB)")

(future-fact "check validations work"); valid? and validate!


(future-fact "incorrectly named attributes are caught"
  (eval `(core/defmodel :ys :fields [{:a :b}])) => throws
  (eval `(core/defmodel :ys :fields [{:name :b :unexpected-field :y}])) => throws)
