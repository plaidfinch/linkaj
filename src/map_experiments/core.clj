(ns map-experiments.core)

(defprotocol Invertible
  "Protocol for a map which can be inverted, preferably in O(1) time."
  (inverse [m] "Returns an invertible map inverted."))

(defprotocol Attributed
  "Protocol for an 'attribute map' which, unlike vanilla maps, associates a key with one or more attribute-value pairs."
  (attr-get [m a-map])
  (attr-find [m a-map]))

(prefer-method
  print-method
  clojure.lang.IPersistentMap
  clojure.lang.IPersistentSet)

(declare inverted-surjection-)

; A SetMap is like a regular map, but forces keys to be sets, and overrides assoc so that it augments the set at that key rather than replacing the value. It's used as a building block for the later constructs.
(deftype SetMap [metadata contents]
  clojure.lang.IPersistentMap
    (assoc [this k v]
           (SetMap. metadata
                    (assoc contents k ((fnil conj #{}) (get contents k) v))))
    (without [this k]
             (SetMap. metadata (dissoc contents k)))
  clojure.lang.Associative
    (containsKey [this k] (contains? contents k))
    (entryAt     [this k] (find contents k))
  clojure.lang.IPersistentCollection
    (count [this] (count contents))
    (cons [this [k v]] (assoc this k v))
    (empty [this] (SetMap. metadata (empty contents)))
    (equiv [this o] (or (and (isa? (class o) SetMap)
                             (= contents (.contents ^SetMap o)))
                        (= contents o)))
  clojure.lang.Seqable
    (seq [this] (seq contents))
  clojure.lang.ILookup
    (valAt [this k]           (get contents k))
    (valAt [this k not-found] (get contents k not-found))
  clojure.lang.MapEquivalence
  clojure.lang.IPersistentSet
    (disjoin [this [k v]]
             (if (not (contains? contents k))
                 this
                 (SetMap. metadata
                          (if (< 1 (count (get contents k)))
                              (assoc contents k (disj (get contents k) v))
                              (dissoc contents k)))))
  clojure.lang.IFn
    (invoke [this k] (get contents k))
  clojure.lang.IObj
    (withMeta [this new-meta] (SetMap. new-meta contents))
  clojure.lang.IMeta
    (meta [this] metadata))

; Map that preserves a bijective property amongst its elements.
(deftype Bijection [metadata active mirror]
  Invertible
    (inverse [this] (Bijection. metadata mirror active))
  clojure.lang.IPersistentMap
    (assoc [this k v]
           (Bijection. metadata
                       (assoc (dissoc active (get mirror v)) k v)
                       (assoc (dissoc mirror (get active k)) v k)))
    (without [this k]
             (disj this [k (get active k)]))
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

; Dual (invertible) SetMap with no restrictions on associations.
(deftype Bipartite [metadata active mirror]
  Invertible
    (inverse [this] (Bipartite. metadata mirror active))
  clojure.lang.IPersistentMap
    (assoc [this k v]
           (conj this [k v]))
    (without [this k]
             (reduce disj this (map (partial vector k) (get active k))))
  clojure.lang.Associative
    (containsKey [this k] (contains? active k))
    (entryAt     [this k] (find active k))
  clojure.lang.IPersistentCollection
    (count [this] (count active))
    (cons [this [k v]]
          (Bipartite. metadata
                      (assoc active k v)
                      (assoc mirror v k)))
    (empty [this] (Bipartite. metadata (empty active) (empty mirror)))
    (equiv [this o] (or (and (isa? (class o) Bipartite)
                             (= active (.active ^Bipartite o)))
                        (= active o)))
  clojure.lang.Seqable
    (seq [this] (seq active))
  clojure.lang.ILookup
    (valAt [this k]           (get active k))
    (valAt [this k not-found] (get active k not-found))
  clojure.lang.MapEquivalence
  clojure.lang.IPersistentSet
    (disjoin [this [k v]]
             (if (not (and (contains? active k) (contains? mirror v)))
                 this
                 (Bipartite. metadata
                             (disj active [k v])
                             (disj mirror [v k]))))
  clojure.lang.IFn
    (invoke [this k] (get active k))
  clojure.lang.IObj
    (withMeta [this new-meta] (Bipartite. new-meta active mirror))
  clojure.lang.IMeta
    (meta [this] metadata))

; Drop-in replacement for normal associative map, with the additional functionality of invertibility. Yields an InvertedSurjection when inverted.
(deftype Surjection [metadata active mirror]
  Invertible
    (inverse [this] (inverted-surjection- metadata mirror active))
  clojure.lang.IPersistentMap
    (assoc [this k v]
           (conj this [k v]))
    (without [this k]
             (disj this [k (get active k)]))
  clojure.lang.Associative
    (containsKey [this k] (contains? active k))
    (entryAt     [this k] (find active k))
  clojure.lang.IPersistentCollection
    (count [this] (count active))
    (cons [this [k v]]
          (Surjection. metadata
                      (assoc active k v)
                      (assoc (dissoc mirror (get active k)) v k)))
    (empty [this] (Surjection. metadata (empty active) (empty mirror)))
    (equiv [this o] (or (and (isa? (class o) Surjection)
                             (= active (.active ^Surjection o)))
                        (= active o)))
  clojure.lang.Seqable
    (seq [this] (seq active))
  clojure.lang.ILookup
    (valAt [this k]           (get active k))
    (valAt [this k not-found] (get active k not-found))
  clojure.lang.MapEquivalence
  clojure.lang.IPersistentSet
    (disjoin [this [k v]]
             (if (not (and (contains? active k) (contains? mirror v)))
                 this
                 (Surjection. metadata
                             (dissoc active k)
                             (disj mirror [v k]))))
  clojure.lang.IFn
    (invoke [this k] (get active k))
  clojure.lang.IObj
    (withMeta [this new-meta] (Surjection. new-meta active mirror))
  clojure.lang.IMeta
    (meta [this] metadata))

; Dual of Surjection. Behaves like a SetMap, except it preserves the surjective property of the original map.
(deftype InvertedSurjection [metadata active mirror]
  Invertible
    (inverse [this] (Surjection. metadata mirror active))
  clojure.lang.IPersistentMap
    (assoc [this k v]
           (conj this [k v]))
    (without [this k]
             (reduce disj this (map (partial vector k) (get active k))))
  clojure.lang.Associative
    (containsKey [this k] (contains? active k))
    (entryAt     [this k] (find active k))
  clojure.lang.IPersistentCollection
    (count [this] (count active))
    (cons [this [k v]]
          (InvertedSurjection. metadata
                               (assoc (dissoc active (get mirror v)) k v)
                               (assoc mirror v k)))
    (empty [this]
           (InvertedSurjection. metadata (empty active) (empty mirror)))
    (equiv [this o] (or (and (isa? (class o) InvertedSurjection)
                             (= active (.active ^InvertedSurjection o)))
                        (= active o)))
  clojure.lang.Seqable
    (seq [this] (seq active))
  clojure.lang.ILookup
    (valAt [this k]           (get active k))
    (valAt [this k not-found] (get active k not-found))
  clojure.lang.MapEquivalence
  clojure.lang.IPersistentSet
    (disjoin [this [k v]]
             (if (not (and (contains? active k) (contains? mirror v)))
                 this
                 (InvertedSurjection. metadata
                                      (disj active [k v])
                                      (dissoc mirror v))))
  clojure.lang.IFn
    (invoke [this k] (get active k))
  clojure.lang.IObj
    (withMeta [this new-meta] (InvertedSurjection. new-meta active mirror))
  clojure.lang.IMeta
    (meta [this] metadata))

; Factory function for SetMaps.
(defn set-map
  ([] (SetMap. nil (hash-map)))
  ([& keyvals]
   (apply assoc (set-map) keyvals)))

; Factory functions for the core datatypes in this file:

(defn bijection
  ([] (Bijection. nil (hash-map) (hash-map)))
  ([& keyvals]
   (apply assoc (bijection) keyvals)))

(defn surjection
  ([] (Surjection. nil (hash-map) (set-map)))
  ([& keyvals]
   (apply assoc (surjection) keyvals)))

(defn bipartite
  ([] (Bipartite. nil (set-map) (set-map)))
  ([& keyvals]
   (apply assoc (bipartite) keyvals)))

; Private factory function for InvertedSurjection. Required because of limitations on coroutined type definitions.
(defn- inverted-surjection-
  ([metadata mirror active] (InvertedSurjection. metadata mirror active)))

; Like dissoc, but does it backward. Works with things implementing the Invertible protocol.
(defn rdissoc
  ([coll & ks]
   (inverse (reduce dissoc (inverse coll) ks))))

; Like assoc, but does it backward. Works with Invertible things.
(defn rassoc
  ([coll & keyvals]
   (inverse (reduce (partial apply assoc)
                    (inverse coll)
                    (partition 2 keyvals)))))

; (defn sorted-bijection-by
;   ([comp-both]
;    (Bijection. nil (sorted-map-by comp-both) (sorted-map-by comp-both)))
;   ([comp-active comp-mirror]
;    (Bijection. nil (sorted-map-by comp-active) (sorted-map-by comp-mirror)))
;   ([comp-active comp-mirror & keyvals]
;    (apply assoc (sorted-bijection-by comp-active comp-mirror) keyvals)))

; (defn sorted-bijection
;   ([] (sorted-bijection-by compare))
;   ([& keyvals]
;    (apply assoc (sorted-bijection) keyvals)))
