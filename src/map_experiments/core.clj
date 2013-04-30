(ns map-experiments.core
  (:import [clojure.lang
              IPersistentMap
              IPersistentSet
              IPersistentCollection
              ILookup
              IFn
              IObj
              IMeta
              Associative
              MapEquivalence
              Seqable]))

(defprotocol Invertible
  "Protocol for a map which can be inverted, preferably in O(1) time."
  (inverse [m] "Returns an invertible map inverted."))

(defprotocol Attributed
  "Protocol for an 'attribute map' which, unlike vanilla maps, associates a key with one or more attribute-value pairs."
  (attr-get [m a-map])
  (attr-find [m a-map]))

(prefer-method
  print-method
  IPersistentMap
  IPersistentSet)

(declare inverted-surjection-)

; A SetMap is like a regular map, but forces keys to be sets, and overrides assoc so that it augments the set at that key rather than replacing the value. It's used as a building block for the later constructs.
(deftype SetMap [metadata contents]
  IPersistentMap
    (assoc [this k v]
           (SetMap. metadata
                    (assoc contents k ((fnil conj #{}) (get contents k) v))))
    (without [this k]
             (SetMap. metadata (dissoc contents k)))
  Associative
    (containsKey [this k] (contains? contents k))
    (entryAt     [this k] (find contents k))
  IPersistentCollection
    (count [this] (count contents))
    (cons [this [k v]] (assoc this k v))
    (empty [this] (SetMap. metadata (empty contents)))
    (equiv [this o] (or (and (isa? (class o) SetMap)
                             (= contents (.contents ^SetMap o)))
                        (= contents o)))
  Seqable (seq [this] (seq contents))
  ILookup
    (valAt [this k]           (get contents k))
    (valAt [this k not-found] (get contents k not-found))
  MapEquivalence
  IPersistentSet
    (disjoin [this [k v]]
             (if (not (contains? contents k))
                 this
                 (SetMap. metadata
                          (if (< 1 (count (get contents k)))
                              (assoc contents k (disj (get contents k) v))
                              (dissoc contents k)))))
  IFn   (invoke [this k] (get contents k))
  IObj  (withMeta [this new-meta] (SetMap. new-meta contents))
  IMeta (meta [this] metadata))

; Invertible map that preserves a bijective property amongst its elements.
(deftype Bijection [metadata
                    ^IPersistentMap active
                    ^IPersistentMap mirror]
  Invertible (inverse [this] (Bijection. metadata mirror active))
  IPersistentMap
    (assoc [this k v]
           (Bijection. metadata
                       (assoc (dissoc active (get mirror v)) k v)
                       (assoc (dissoc mirror (get active k)) v k)))
    (without [this k] (disj this [k (get active k)]))
  Associative
    (containsKey [this k] (contains? active k))
    (entryAt     [this k] (find active k))
  IPersistentCollection
    (count [this]       (count active))
    (cons  [this [k v]] (assoc this k v))
    (empty [this]       (Bijection. metadata (empty active) (empty mirror)))
    (equiv [this o]     (or (and (isa? (class o) Bijection)
                                 (= active (.active ^Bijection o)))
                            (= active o)))
  Seqable (seq [this] (seq active))
  ILookup
    (valAt [this k]           (get active k))
    (valAt [this k not-found] (get active k not-found))
  MapEquivalence
  IPersistentSet
    (disjoin [this [k v]]
             (if (and (contains? active k) (contains? mirror v))
                 (Bijection. metadata (dissoc active k) (dissoc mirror v))
                 this))
  IFn   (invoke [this k] (get active k))
  IObj  (withMeta [this new-meta] (Bijection. new-meta active mirror))
  IMeta (meta [this] metadata))

; Dual (invertible) SetMap with no restrictions on associations.
(deftype Bipartite [metadata
                    ^SetMap active
                    ^SetMap mirror]
  Invertible (inverse [this] (Bipartite. metadata mirror active))
  IPersistentMap
    (assoc [this k v]
           (conj this [k v]))
    (without [this k]
             (reduce disj this (map (partial vector k) (get active k))))
  Associative
    (containsKey [this k] (contains? active k))
    (entryAt     [this k] (find active k))
  IPersistentCollection
    (count [this] (count active))
    (cons [this [k v]]
          (Bipartite. metadata
                      (assoc active k v)
                      (assoc mirror v k)))
    (empty [this] (Bipartite. metadata (empty active) (empty mirror)))
    (equiv [this o] (or (and (isa? (class o) Bipartite)
                             (= active (.active ^Bipartite o)))
                        (= active o)))
  Seqable (seq [this] (seq active))
  ILookup
    (valAt [this k]           (get active k))
    (valAt [this k not-found] (get active k not-found))
  MapEquivalence
  IPersistentSet
    (disjoin [this [k v]]
             (if (not (and (contains? active k) (contains? mirror v)))
                 this
                 (Bipartite. metadata
                             (disj active [k v])
                             (disj mirror [v k]))))
  IFn   (invoke [this k] (get active k))
  IObj  (withMeta [this new-meta] (Bipartite. new-meta active mirror))
  IMeta (meta [this] metadata))

; Drop-in replacement for normal associative map, with the additional functionality of invertibility. Yields an InvertedSurjection when inverted.
(deftype Surjection [metadata
                     ^IPersistentMap active
                     ^SetMap mirror]
  Invertible (inverse [this] (inverted-surjection- metadata mirror active))
  IPersistentMap
    (assoc [this k v]
           (conj this [k v]))
    (without [this k]
             (disj this [k (get active k)]))
  Associative
    (containsKey [this k] (contains? active k))
    (entryAt     [this k] (find active k))
  IPersistentCollection
    (count [this] (count active))
    (cons [this [k v]]
          (Surjection. metadata
                      (assoc active k v)
                      (assoc (dissoc mirror (get active k)) v k)))
    (empty [this] (Surjection. metadata (empty active) (empty mirror)))
    (equiv [this o] (or (and (isa? (class o) Surjection)
                             (= active (.active ^Surjection o)))
                        (= active o)))
  Seqable (seq [this] (seq active))
  ILookup
    (valAt [this k]           (get active k))
    (valAt [this k not-found] (get active k not-found))
  MapEquivalence
  IPersistentSet
    (disjoin [this [k v]]
             (if (not (and (contains? active k) (contains? mirror v)))
                 this
                 (Surjection. metadata
                             (dissoc active k)
                             (disj mirror [v k]))))
  IFn   (invoke [this k] (get active k))
  IObj  (withMeta [this new-meta] (Surjection. new-meta active mirror))
  IMeta (meta [this] metadata))

; Dual of Surjection. Behaves like a SetMap, except it preserves the surjective property of the original map. Yields a Surjection when inverted.
(deftype InvertedSurjection [metadata
                             ^SetMap active
                             ^IPersistentMap mirror]
  Invertible (inverse [this] (Surjection. metadata mirror active))
  IPersistentMap
    (assoc [this k v]
           (conj this [k v]))
    (without [this k]
             (reduce disj this (map (partial vector k) (get active k))))
  Associative
    (containsKey [this k] (contains? active k))
    (entryAt     [this k] (find active k))
  IPersistentCollection
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
  Seqable (seq [this] (seq active))
  ILookup
    (valAt [this k]           (get active k))
    (valAt [this k not-found] (get active k not-found))
  MapEquivalence
  IPersistentSet
    (disjoin [this [k v]]
             (if (not (and (contains? active k) (contains? mirror v)))
                 this
                 (InvertedSurjection. metadata
                                      (disj active [k v])
                                      (dissoc mirror v))))
  IFn  (invoke [this k] (get active k))
  IObj (withMeta [this new-meta] (InvertedSurjection. new-meta active mirror))
  IMeta (meta [this] metadata))

; Factory functions for the core datatypes in this file:

(defn set-map
  ([] (SetMap. nil (hash-map)))
  ([& keyvals]
   (apply assoc (set-map) keyvals)))

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
