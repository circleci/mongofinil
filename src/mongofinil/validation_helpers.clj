(ns mongofinil.validation-helpers
  (:require [clojure.set]
            [clojure.string]

            [mongofinil.helpers :refer (ref?)]))

(defmacro require-predicate [f & msg]
  `(fn [o#]
    (when-not (~f o#)
      (or ~msg (format "Object must pass predicate %s" (quote ~f))))))

(defmacro col-predicate
  "Require that f, a predicate, returns truthy when passed (get obj col)"
  [col f & [msg]]
  `(fn [o#]
     (when-not (~f (get o# ~col))
       (or ~msg (format "field %s must pass predicate %s" (quote ~col) (quote ~f))))))

(defn require-key* [o k]
  ;;{:post [(do (println "require-key* post:" k "=" %) true)]}
  (when-not (contains? o k)
    (format "column %s is required" k)))

(defn require-key [k]
  (fn [o]
    (require-key* o k)))

(defn map-predicate
  "Calls a validation fn on each item in coll, returning the first
  call to return an error string"
  [f s]
  (->> s
       (map f)
       (filter identity)
       (first)))

(defn require-keys
  "Validates that the map contains all of the keys"
  [keys]
  (fn [obj]
    (map-predicate #(require-key* obj %) keys)))

(defn allow-keys
  "Validates that the map MAY contain keys listed, and no keys not listed"
  [allowed-keys & [msg]]
  (fn [obj]
    (let [extra-keys (clojure.set/difference (set (keys obj)) (set allowed-keys))]
      (when (seq extra-keys)
        (or msg (format "keys %s are not allowed" (clojure.string/join "," extra-keys)))))))

(defn key-type
  "Validates that the column is of the specified class"
  [k cls]
  (fn [o]
    (require-predicate #(instance? cls (get o k)) (format "key %s must be a %s" k cls))))

(defn key-types
  "Takes a map of keys to classes. Validates that each key in map is of the specified class."
  [ks]
  (fn [o]
    (map-predicate #((key-type %1 %2) o) ks)))

(defn is-map? [& [msg]]
  (require-predicate map? msg))

(defn is-ref? [& [msg]]
  (require-predicate ref? msg))

(defn maybe
  "HOF, takes a predicate, and returns a fn that is true if argument passed in is nil, or passes the predicate.

  ((maybe integer?) 3) => true
  ((maybe integer?) nil) => true
  ((maybe integer?) :bogus) => false"
  [f]
  (fn [x]
    (or (nil? x) (f x))))
