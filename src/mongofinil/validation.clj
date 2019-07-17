(ns mongofinil.validation
  (:require [mongofinil.helpers :refer (throw-if-not seq1)]))

(defn validate
  "validates an object. validation-seq is a seq of fns. Each fn takes one argument, the object to be validated, and returns nil on success, or a string containing a helpful error message on failure.

 (validate [(require-pred map?)
            (require-keys :type :foo)
            (require-col-pred :foo int?)] m) "
  [validation-seq obj]
  (->>
   (for [v-fn (seq1 validation-seq)]
     (do
       (let [resp (v-fn obj)]
         (throw-if-not (or (nil? resp) (string? resp)) "Validation fn %s, expected nil or string?, got %s" v-fn (class resp))
         resp)))
   (filter identity)
   (first)))

(defn valid?
  "Similar to validation-error, but returns a boolean"
  [validation-seq obj]
  (not (string? (validate validation-seq obj))))

(defn validate!
  "Similar to validation-error, but returns the obj being validated, or throws on failure"
  [validation-seq obj]
  (if-let [resp (validate validation-seq obj)]
    (throw (Exception. resp))
    obj))
