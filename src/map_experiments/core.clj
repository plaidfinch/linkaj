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

; http://clojuredocs.org/clojure_contrib/clojure.contrib.types/deftype-
(defmacro deftype-
  "Same as deftype but the constructor is private."
  [type-tag constructor-name & optional]
  `(deftype ~type-tag
     ~(vary-meta constructor-name assoc :private true)
     ~@optional))

(defprotocol Invertible
  "Protocol for a map which can be inverted, preferably in O(1) time."
  (inverse [m] "Returns an invertible map inverted."))

(defprotocol Attributed
  "Protocol for an 'attribute map' which, unlike vanilla maps, associates a key with one or more attribute-value pairs."
  (attr-get [m a-map])
  (attr-find [m a-map]))

; Always default to mappy printing for things which are both mappy and setty.
(prefer-method
  print-method
  IPersistentMap
  IPersistentSet)

; Forward declaration of coroutined constructors for surjection types.
(declare inverted-surjection- surjection-)

; A SetMap is like a regular map, but forces keys to be sets, and overrides assoc so that it augments the set at that key rather than replacing the value. It's used as a building block for the later constructs.
(deftype- SetMap [metadata contents]
  IPersistentMap
    (assoc [this k v]
           (SetMap. metadata (assoc contents k ((fnil conj #{}) (get contents k) v))))
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
             (if (contains? contents k)
                 (SetMap. metadata
                          (if (< 1 (count (get contents k)))
                              (assoc contents k (disj (get contents k) v))
                              (dissoc contents k)))
                 this))
  IFn   (invoke [this k] (get contents k))
  IObj  (withMeta [this new-meta] (SetMap. new-meta contents))
  IMeta (meta [this] metadata))

; Invertible map that preserves a bijective property amongst its elements.
(deftype- Bijection [metadata
                    ^IPersistentMap active
                    ^IPersistentMap mirror]
  Invertible (inverse [this] (Bijection. metadata mirror active))
  IPersistentMap
    (assoc [this k v]
           (Bijection. metadata
                       (assoc (dissoc active (get mirror v)) k v)
                       (assoc (dissoc mirror (get active k)) v k)))
    (without [this k] (disj this [k (get active k)]))
  IPersistentCollection
    (cons  [this [k v]] (assoc this k v))
    (equiv [this o] 
           (or (and (isa? (class o) Bijection)
                    (= active (.active ^Bijection o)))
               (= active o)))
    (count [this] (count active))
    (empty [this] (Bijection. metadata (empty active) (empty mirror)))
  IPersistentSet
    (disjoin [this [k v]]
             (if (and (contains? active k) (contains? mirror v))
                 (Bijection. metadata (dissoc active k) (dissoc mirror v))
                 this))
  IObj (withMeta [this new-meta] (Bijection. new-meta active mirror))
  ; Everything below here is the same for all mirrored maps.
  Associative
    (containsKey [this k] (contains? active k))
    (entryAt     [this k] (find active k))
  ILookup
    (valAt [this k]           (get active k))
    (valAt [this k not-found] (get active k not-found))
  Seqable (seq    [this]   (seq active))
  IFn     (invoke [this k] (get active k))
  IMeta   (meta   [this]   metadata)
  MapEquivalence)

; Dual (invertible) SetMap with no restrictions on associations; that is to say, a bipartite graph.
(deftype- Bipartite [metadata
                    ^SetMap active
                    ^SetMap mirror]
  Invertible (inverse [this] (Bipartite. metadata mirror active))
  IPersistentMap
    (assoc   [this k v] (conj this [k v]))
    (without [this k]   (reduce disj this (map (partial vector k) (get active k))))
  IPersistentCollection
    (cons [this [k v]]
          (Bipartite. metadata (assoc active k v) (assoc mirror v k)))
    (equiv [this o]
           (or (and (isa? (class o) Bipartite)
                    (= active (.active ^Bipartite o)))
               (= active o)))
    (count [this] (count active))
    (empty [this] (Bipartite. metadata (empty active) (empty mirror)))
  IPersistentSet
    (disjoin [this [k v]]
             (if (and (contains? active k) (contains? mirror v))
                 (Bipartite. metadata (disj active [k v]) (disj mirror [v k]))
                 this))
  IObj (withMeta [this new-meta] (Bipartite. new-meta active mirror))
  ; Everything below here is the same for all mirrored maps.
  Associative
    (containsKey [this k] (contains? active k))
    (entryAt     [this k] (find active k))
  ILookup
    (valAt [this k]           (get active k))
    (valAt [this k not-found] (get active k not-found))
  Seqable (seq    [this]   (seq active))
  IFn     (invoke [this k] (get active k))
  IMeta   (meta   [this]   metadata)
  MapEquivalence)

; Drop-in replacement for normal associative map, with the additional functionality of invertibility. Yields an InvertedSurjection when inverted.
(deftype- Surjection [metadata
                     ^IPersistentMap active
                     ^SetMap mirror]
  Invertible (inverse [this] (inverted-surjection- metadata mirror active))
  IPersistentMap
    (assoc   [this k v] (conj this [k v]))
    (without [this k]   (disj this [k (get active k)]))
  IPersistentCollection
    (cons [this [k v]]
          (Surjection. metadata
                       (assoc active k v)
                       (assoc (disj mirror [(get active k) k]) v k)))
    (equiv [this o]
           (or (and (isa? (class o) Surjection)
                    (= active (.active ^Surjection o)))
               (= active o)))
    (count [this] (count active))
    (empty [this] (Surjection. metadata (empty active) (empty mirror)))
  IPersistentSet
    (disjoin [this [k v]]
             (if (and (contains? active k) (contains? mirror v))
                 (Surjection. metadata (dissoc active k) (disj mirror [v k]))
                 this))
  IObj (withMeta [this new-meta] (Surjection. new-meta active mirror))
  ; Everything below here is the same for all mirrored maps.
  Associative
    (containsKey [this k] (contains? active k))
    (entryAt     [this k] (find active k))
  ILookup
    (valAt [this k]           (get active k))
    (valAt [this k not-found] (get active k not-found))
  Seqable (seq    [this]   (seq active))
  IFn     (invoke [this k] (get active k))
  IMeta   (meta   [this]   metadata)
  MapEquivalence)

; Dual of Surjection. Behaves like a SetMap, except it preserves the surjective property of the original map. Yields a Surjection when inverted.
(deftype- InvertedSurjection [metadata
                             ^SetMap active
                             ^IPersistentMap mirror]
  Invertible (inverse [this] (surjection- metadata mirror active))
  IPersistentMap
    (assoc   [this k v] (conj this [k v]))
    (without [this k]   (reduce disj this (map (partial vector k) (get active k))))
  IPersistentCollection
    (cons [this [k v]]
          (InvertedSurjection. metadata
                               (assoc (disj active [(get mirror v) v]) k v)
                               (assoc mirror v k)))
    (equiv [this o]
           (or (and (isa? (class o) InvertedSurjection)
                    (= active (.active ^InvertedSurjection o)))
               (= active o)))
    (count [this] (count active))
    (empty [this] (InvertedSurjection. metadata (empty active) (empty mirror)))
  IPersistentSet
    (disjoin [this [k v]]
             (if (and (contains? active k) (contains? mirror v))
                 (InvertedSurjection. metadata (disj active [k v]) (dissoc mirror v)))
             this)
  IObj (withMeta [this new-meta] (InvertedSurjection. new-meta active mirror))
  ; Everything below here is the same for all mirrored maps.
  Associative
    (containsKey [this k] (contains? active k))
    (entryAt     [this k] (find active k))
  ILookup
    (valAt [this k]           (get active k))
    (valAt [this k not-found] (get active k not-found))
  Seqable (seq    [this]   (seq active))
  IFn     (invoke [this k] (get active k))
  IMeta   (meta   [this]   metadata)
  MapEquivalence)

; Private factory functions for InvertedSurjection and Surjection. Required because of limitations on coroutined type definitions.
(defn- inverted-surjection-
  ([metadata active mirror] (InvertedSurjection. metadata active mirror)))
(defn- surjection-
  ([metadata active mirror] (Surjection. metadata active mirror)))

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

; Like dissoc, but does it backward. Works with things implementing the Invertible protocol.
(defn rdissoc
  ([coll & ks]
   (inverse (apply dissoc (inverse coll) ks))))

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
