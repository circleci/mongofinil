(ns mongofinil.test-refs
  (:use midje.sweet)
  (:require [somnium.congomongo :as congo])
  (:require [mongofinil.core :as core])
  (:use [mongofinil.helpers :only (ref?)])
  (:require [mongofinil.testing-utils :as utils]))

(utils/setup-test-db)
(utils/setup-midje)

(core/defmodel :xs
  :use-refs true
  :fields [;; simple
           {:name :x :findable true}
           {:name :y :findable false}
           {:name :z}
           {:name :w :findable true}

           ;; default
           {:name :dx :default 5}
           {:name :dy :default (fn [b] 6)}
           {:name :dz :default (fn [b] (-> b :x))}

           {:name :disx :transient true}

           ;; ordered defaults
           {:name :def1 :default 5}
           {:name :def2 :default (fn [b] (inc (:def1 b)))}
           {:name :def3 :default (fn [b] (inc (:def2 b)))}
           {:name :def4 :default (fn [b] (:x b))}

           ;; transient
           {:name :disx :transient true}

           ;; keyword
           {:name :kw :keyword true}

           ;; validation
           {:name :valid-pos :default 5 :validator (fn [row] (when-not (pos? (:valid-pos row)) "Should be positive"))}]
  :validations [(fn [row] (when false "failing"))])

(fact "row findable functions are created and work"
  (let [obj1 (create! {:x 1 :y 2 :z 3 :w 4})
        obj2 (create! {:x 2 :y 3 :z 4 :w 5})]

    ;; create worked and added an id
    obj1 =not=> nil
    obj2 =not=> nil
    (contains? @obj1 :_id) => true
    (contains? @obj2 :_id) => true
    @obj1 => (contains {:x 1 :y 2 :z 3 :w 4})
    @obj2 => (contains {:x 2 :y 3 :z 4 :w 5})

    ;; success
    @(find-one-by-x 1) => (contains @obj1)
    @(find-one-by-x! 1) => (contains @obj1)
    @(find-one-by-w 4) => (contains @obj1)
    @(find-one-by-w! 4) => (contains @obj1)
    @(find-one-by-x 2) => (contains @obj2)
    @(find-one-by-x! 2) => (contains @obj2)
    @(find-one-by-w 5) => (contains @obj2)
    @(find-one-by-w! 5) => (contains @obj2)

    @(first (find-by-x 1)) => (contains @obj1)
    @(first (find-by-x! 1)) => (contains @obj1)
    @(first (find-by-w 4)) => (contains @obj1)
    @(first (find-by-w! 4)) => (contains @obj1)
    @(first (find-by-x 2)) => (contains @obj2)
    @(first (find-by-x! 2)) => (contains @obj2)
    @(first (find-by-w 5)) => (contains @obj2)
    @(first (find-by-w! 5)) => (contains @obj2)

    ;; failure
    (resolve 'find-one-by-y) => nil
    (resolve 'find-one-by-y!) => nil

    ;; empty
    (find-one-by-x 3) => nil
    (find-one-by-w 2) => nil
    (find-by-w 2) => '()
    (find-one-by-x! 3) => (throws Exception "Couldn't find row with :x=3 on collection :xs")
    (find-one-by-w! 2) => (throws Exception "Couldn't find row with :w=2 on collection :xs")))

(fact "find-by-id works"
  (let [obj (create! {:x 5 :y 6})
        id (:_id @obj)]
    @(find-by-id obj) => (contains @obj)
    @(find-by-id id) => (contains @obj)
    @(find-by-id (str id)) => (contains @obj)
    @(find-by-id (congo/object-id (str id))) => (contains @obj)))

(fact "find-by-ids works"
  (let [obj1 (create! {:x 5 :y 6})
        obj2 (create! {:x 5 :y 6})
        ids (map :_id [@obj1 @obj2])]
    (map deref (find-by-ids [obj1 obj2])) => (list @obj1 @obj2)
    (map deref (find-by-ids ids)) => (list @obj1 @obj2)
    (map deref (find-by-ids (map str ids))) => (list @obj1 @obj2)))

(fact "find-one works"
  (create! {:x 5 :y 6})
  @(find-one) => (contains {:x 5 :y 6})
  @(find-one {:y 6}) => (contains {:x 5 :y 6})
  (find-one {:y 7}) => nil)

(fact "keyword works"
  @(create! {:x 5 :kw :asd}) => (contains {:x 5 :kw :asd})
  @(find-one) => (contains {:x 5 :kw :asd}))


(fact "apply-defaults works"
  (core/apply-defaults [[ :x 5] [:y (fn [v] 6)] [:z 10]] {:z 9}) => (contains {:x 5 :y 6 :z 9}))


(fact "default works on creation"
  @(create! {:x 7}) => (contains {:dx 5 :dy 6 :dz 7})
  @(nu {:x 7}) => (contains {:dx 5 :dy 6 :dz 7}))

(fact "default works on loading"
  (congo/insert! :xs {:x 22})
  @(find-one-by-x! 22) => (contains {:dx 5 :dy 6 :dz 22})
  @(find-one-by-x 22) => (contains {:dx 5 :dy 6 :dz 22}))

(fact "defaults work in order"
  @(create! {:x 11}) => (contains {:x 11 :def1 5 :def2 6 :def3 7 :def4 11}))

(fact "no nil defaults"
  @(create! {:x 5}) =not=> (contains {:y anything}))


(fact "transient causes things not to be saved to the DB"
  @(create! {:disx 5 :x 12}) => (contains {:disx 5})
  @(find-one-by-x 12) =not=> (contains {:disx 5}))


(fact "ensure set works as planned"
  ;; add and check expected values
  (create! {:a "b" :c "d"})
  (let [old (find-one)]
    @old => (contains {:a "b" :c "d"})

    ;; set and check expected values
    (let [result (set-fields! old {:a "x" :e "f"})
          count (find-count)
          new (find-one)]
      count => 1
     @result => @new
     @new => (contains {:a "x" :c "d" :e "f"}))))

(fact "replace! works"
  (find-count) => 0
  (let [x (create! {:a :b :x :w})]
    (replace! x {:c :d :a :B}))
  (find-count) => 1
  @(find-one) => (contains {:c "d" :a "B"})
  @(find-one) =not=> (contains {:x "w"}))

(fact "dont try to serialize transiented"
  (create! {:disx (fn [] "x")}))

(fact "check validations work"
  (let [x (create! {:valid-pos 5})]
    (valid? x) => true
    (validate! x) => x))

(fact "check validations fail properly"
  (create! {:valid-pos -1}) => (throws Exception "Should be positive"))

(fact "all works"
  (all) => (list)

  (create! {:x 5 :y 6})
  (count (all)) => 1

  (create! {:y 7 :z 8})
  (let [result (all)]
    result => seq?
    (count result) => 2
    result => (contains [ref? ref?])
    (-> result first deref) => (contains {:x 5 :y 6})
    (-> result last deref) => (contains {:y 7 :z 8})))

(fact "where works"
  (where {:x 5}) => (list)

  (create! {:x 5 :y 6})
  (-> {:x 5} where first deref) => (contains {:x 5 :y 6})
  (where {:x 6}) => (list)

  (create! {:y 7 :z 8})
  (let [result (where {:x 5 :y 6})]
    result => seq?
    (count result) => 1
    @(first result) => (contains {:x 5 :y 6}))

  ;; check defaults
  (count (where {:dx 5})) => 2)

(fact "`all and :keywords work together"
  (create! {:kw "state"})
  (-> (all) first deref :kw) => :state)

(fact "find doesn't return refs pointing at nil"
  (find-one {:x :bogus}) => nil
  (seq (where {:x :bogus})) => nil)

(future-fact "transient doesnt stop things being loaded from the DB"
             (congo/insert! :xs {:disx 55 :x 55})
             (find-by-x 55) => (contains {:disx 55}))

(future-fact "refresh function exists and works (refreshes from DB")


(future-fact "incorrectly named attributes are caught"
  (eval `(core/defmodel :ys :fields [{:a :b}])) => throws
  (eval `(core/defmodel :ys :fields [{:name :b :unexpected-field :y}])) => throws)

(future-fact "calling functions with the wrong signatures should give slightly useful error messages")