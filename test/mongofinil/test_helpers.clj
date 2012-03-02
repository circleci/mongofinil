(ns mongofinil.test-helpers
  (:use [midje.sweet])
  (:require [somnium.congomongo :as congo]))

;;; Not helpers for tests, test for helpers


(fact "ensure set works as planned"
  ;; add and check expected values
  (congo/insert! :some_coll {:a "b" :c "d"})
  (let [old (congo/fetch-one :some_coll)]
    (-> old :a) => "b"
    (-> old :c) => "d"

    ;; set and check expected values

    (let [result (set-fields :some_coll (-> old :_id) :a "x" :e "f")
          count (congo/fetch-count :some_coll)
          new (congo/fetch-one :some_coll)]
      result => old
      (-> new :a) => "x"
      (-> new :c) => "d"
      (-> new :e) => "f"
      count => 1)))
