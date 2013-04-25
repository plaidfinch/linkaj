(ns map-experiments.core)

; http://clojuredocs.org/clojure_contrib/clojure.contrib.types/deftype-
(defmacro deftype-
  "Same as deftype but the constructor is private."
  [type-tag constructor-name & optional]
  `(deftype ~type-tag
     ~(vary-meta constructor-name assoc :private true)
     ~@optional))

(defprotocol Invertible
  "Protocol for a map which can be inverted, preferably in O(1) time."
  (inverse [m] "Returns an invertible map inverted. Should be in O(1) time in proper implementations."))

(deftype Bijection [metadata active mirror]
  Invertible
    (inverse [this] (Bijection. metadata mirror active))
  clojure.lang.IPersistentMap
    (assoc [this k v]
           (Bijection. metadata
                       (assoc (dissoc active (get mirror v)) k v)
                       (assoc (dissoc mirror (get active k)) v k)))
    (without [this k]
             (Bijection. metadata
                         (dissoc active k)
                         (dissoc mirror (get active k))))
  clojure.lang.Associative
    (containsKey [this k] (contains? active k))
    (entryAt     [this k] (find active k))
  clojure.lang.IPersistentCollection
    (count [this]       (count active))
    (cons  [this [k v]] (assoc this k v))
    (empty [this]       (Bijection. metadata (empty active) (empty mirror)))
    (equiv [this o]     (or (and (isa? (class o) Bijection)
                                 (= active (.active ^Bijection o)))
                            (= active o)))
  clojure.lang.Seqable
    (seq [this] (seq active))
  clojure.lang.ILookup
    (valAt [this k]           (get active k))
    (valAt [this k not-found] (get active k not-found))
  clojure.lang.MapEquivalence
  clojure.lang.IPersistentSet
    (disjoin [this [k v]]
             (if (and (contains? active k) (contains? mirror v))
                 (Bijection. metadata (dissoc active k) (dissoc mirror v))
                 this))
  clojure.lang.IFn
    (invoke [this k] (get active k))
  clojure.lang.IObj
    (withMeta [this new-meta] (Bijection. new-meta active mirror))
  clojure.lang.IMeta
    (meta [this] metadata))

(prefer-method
  print-method
  clojure.lang.IPersistentMap
  clojure.lang.IPersistentSet)

(defn bijection
  ([] (Bijection. nil {} {}))
  ([& keyvals]
   {:pre [(even? (count keyvals))]}
   (reduce (partial apply assoc)
           (bijection)
           (partition 2 keyvals))))

(defn sorted-bijection-by
  ([comp-both]
   (Bijection. nil (sorted-map-by comp-both) (sorted-map-by comp-both)))
  ([comp-active comp-mirror]
   (Bijection. nil (sorted-map-by comp-active) (sorted-map-by comp-mirror)))
  ([comp-active comp-mirror & keyvals]
   {:pre [(even? (count keyvals))]}
   (reduce (partial apply assoc)
           (sorted-bijection-by comp-active comp-mirror)
           (partition 2 keyvals))))

(defn sorted-bijection
  ([] (sorted-bijection-by compare))
  ([& keyvals]
   {:pre [(even? (count keyvals))]}
   (reduce (partial apply assoc)
           (sorted-bijection)
           (partition 2 keyvals))))

(defn rdissoc
  ([coll & ks]
   (inverse (reduce dissoc (inverse coll) ks))))

(defn rassoc
  ([coll & keyvals]
   (inverse (reduce (partial apply assoc) (inverse coll) (partition 2 keyvals)))))
