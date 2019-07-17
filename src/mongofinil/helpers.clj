(ns mongofinil.helpers
  (:import org.bson.types.ObjectId))

(defn ref?
  "Returns true if val is a ref, false otherwise"
  [val] (instance? clojure.lang.IRef val))

(defn seq1
  "Converts a normal seq, with chunking behavior, to one-at-a-time. See http://blog.fogus.me/2010/01/22/de-chunkifying-sequences-in-clojure/"
  [#^clojure.lang.ISeq s]
  (reify clojure.lang.ISeq
    (first [_] (.first s))
    (more [_] (seq1 (.more s)))
    (next [_] (let [sn (.next s)] (and sn (seq1 sn))))
    (seq [_] (let [ss (.seq s)] (and ss (seq1 ss))))
    (count [_] (.count s))
    (cons [_ o] (.cons s o))
    (empty [_] (.empty s))
    (equiv [_ o] (.equiv s o))))

(defn throwf [& args]
  (throw (Exception. (apply format args))))

(defn eager-map [& args]
  (doall (apply map args)))

(defmacro throw-if [test & format-args]
  `(if ~test
     (throwf ~@format-args)
     ~test))

(defmacro throw-if-not [test & format-args]
  `(if (not ~test)
     (throwf ~@format-args)
     ~test))

(defmacro assert!
  "Asserts expr is truthy. Returns expr on success, or throws msg"
  [expr & msg]
  `(let [r# ~expr]
     (throw-if-not r# ~@(or msg ["%s returned %s" ~expr expr]))
     r#))

(defn object-id
  "Generate a new object id, return it."
  [& [id]]
  (if id
    (do
      (throw-if-not (string? id) "id must be a string")
      (ObjectId. id))
    (ObjectId.)))

(defn object-id?
  "True if o is an object-id"
  [o]
  (instance? org.bson.types.ObjectId o))

(defn coerce-object-id
  "Casts id from a string to an ObjectId if necessary"
  [id]
  (if (object-id? id)
    id
    (object-id id)))

(defn has-object-id?
  "True if map m has a mongo id"
  [m]
  (boolean (-> m :_id)))

(defn ensure-object-id
  "Adds a mongo id to m, a clojure map, if it doesn't have one."
  [_coll m]
  {:post [(identity %)]}
  (if (not (has-object-id? m))
    (assoc m :_id (object-id))
    m))

(defn ensure-object-id-ref
  "Same as ensure-object-id, but works on refs of maps."
  [coll m]
  (dosync
   (when (not (has-object-id? @m))
     (alter m (constantly (ensure-object-id coll @m))))))
