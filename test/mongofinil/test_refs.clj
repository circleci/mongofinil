(ns mongofinil.test-refs
  (:use midje.sweet)
  (:require [somnium.congomongo :as congo])
  (:require [mongofinil.core :as core])
  (:require [mongofinil.testing-utils :as utils])
  (:use [mongofinil.helpers :only (ref?)])
  (:import org.bson.types.ObjectId))

(defn initializer []
  (congo/add-index! :xs [:a])
  (congo/add-index! :xs [:w])
  (congo/add-index! :xs [:x])
  (congo/add-index! :xs [:dx])
  (congo/add-index! :xs [:bogus])
  (congo/add-index! :xs [:y]))

(utils/setup-test-db)
(utils/setup-midje (initializer))

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

           ;; ordered defaults
           {:name :def1 :default 5}
           {:name :def2 :default (fn [b] (inc (:def1 b)))}
           {:name :def3 :default (fn [b] (inc (:def2 b)))}
           {:name :def4 :default (fn [b] (:x b))}

           ;; transient
           {:name :disx :transient true}

           ;; keyword
           {:name :kw :keyword true}

           ;; unique
           {:name :unique1} ;; for congo interop, not a feature

           ;; validation
           {:name :valid-pos :default 5 :validator (fn [row] (when-not (pos? (:valid-pos row)) "Should be positive"))}]
  :hooks {:update {:pre identity
                   :post identity}
          :load {:post identity
                 :pre identity}}
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
    (find-by-id obj) => obj
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
  @(find-one :where {:y 6}) => (contains {:x 5 :y 6})
  (find-one :where {:y 7}) => nil)

(fact "keyword works"
  @(create! {:x 5 :kw :asd}) => (contains {:x 5 :kw :asd})
  @(find-one) => (contains {:x 5 :kw :asd}))


(fact "apply-defaults works"
  (core/apply-defaults [[:x 5] [:y (fn [v] 6)] [:z 10]] {:z 9} nil) => (just {:x 5 :y 6 :z 9}))

(fact "apply-defaults works with only"
  (core/apply-defaults [[:x 5] [:y (fn [v] 6)] [:z 10]] {:z 9} [:z]) => (just {:z 9}))


(fact "default works on creation"
  @(create! {:x 7}) => (contains {:dx 5 :dy 6 :dz 7})
  @(nu {:x 7}) => (contains {:dx 5 :dy 6 :dz 7}))

(fact "default works on loading"
  (congo/insert! :xs {:x 22})
  @(find-one-by-x! 22) => (contains {:dx 5 :dy 6 :dz 22})
  @(find-one-by-x 22) => (contains {:dx 5 :dy 6 :dz 22}))

(fact "defaults work in order"
  @(create! {:x 11}) => (contains {:x 11 :def1 5 :def2 6 :def3 7 :def4 11}))

(fact "defaults apply to nil"
  @(create! {:x 5}) => (contains {:y nil}))

(fact "transient causes things not to be saved to the DB"
  @(create! {:disx 5 :x 12}) => (contains {:disx 5})
  @(find-one-by-x 12) =not=> (contains {:disx 5}))


(fact "ensure find-and-modify! works as planned"
  (let [original (create! {:a "b" :c "d" :e "f"})
        r0 (find-and-modify! {:a (:a @original)} {:$unset {:c true}})
        r1 (find-and-modify! {:a (:a @original)} {:$unset {:e true}} :return-new? true)
        r2 (find-one)]
    @r0 => (contains {:c anything})
    @r1 =not=> (contains {:e anything})
    @r2 =not=> (contains {:c anything :e anything})))

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
  (create! {:disx (fn [] "x")}) => anything)

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

(fact "only works"
  (create! {:x 5 :y 6 :z 7})
  @(find-one :only [:x]) => (just {:x 5 :_id anything}))

(fact "`all and :keywords work together"
  (create! {:kw "state"})
  (-> (all) first deref :kw) => :state)

(fact "save! works"
  (let [row (create! {:x 7})]
    (dosync (alter row merge {:y 45 :kw :g}))
    (let [saved (save! row)]
      @saved => (contains {:y 45 :kw :g})
      @(find-one) => (contains {:y 45 :kw :g}))))


(future-fact "validators cause save to fail coreectly")
(future-fact "validators dont prevent valid objects from saving")

(future-fact "validators cause set-fields to fail coreectly")
(future-fact "validators dont prevent valid objects from having their fields set")

(future-fact "validators cause replace! to fail coreectly")
(future-fact "validators dont prevent valid objects from being replaced")


(fact "find-by-id has an error with an invalid id causes an error"
  (find-by-id "") => (throws Exception #"Got empty string")

  (find-by-ids [""]) => (throws Exception #"Got empty string")
  (find-by-ids ["012345678901234568790123" nil]) => (throws Exception #"Expected id, got nil"))

(fact "push! works"
  (let [orig (create! {:a []})
        new (push! orig :a "b")
        refound (find-one)]
    (:a @orig) => ["b"] ; push changed it
    (:a @new) => ["b"]
    (:a @refound) => ["b"]))

(fact "pull! works"
    (let [orig (create! {:a ["a" "b" "c"]})
        new (pull! orig :a "b")
        refound (find-one)]
    (:a @orig) => ["a" "c"] ; changed by push
    (:a @new) => ["a" "c"]
    (:a @refound) => ["a" "c"]))

(fact "find-one doesn't return bogus refs when there's no object"
  (find-one :where {:bogus 789}) => nil)

(future-fact "transient doesnt stop things being loaded from the DB"
             (congo/insert! :xs {:disx 55 :x 55})
             (find-by-x 55) => (contains {:disx 55}))

(future-fact "refresh function exists and works (refreshes from DB")


(future-fact "incorrectly named attributes are caught"
             (eval `(core/defmodel :ys :fields [{:a :b}])) => (throws Exception)
             (eval `(core/defmodel :ys :fields [{:name :b :unexpected-field :y}])) => (throws Exception))

(future-fact "calling functions with the wrong signatures should give slightly useful error messages")
