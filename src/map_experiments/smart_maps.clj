(ns map-experiments.smart-maps
  (:import [clojure.lang
              IPersistentMap IPersistentSet IPersistentCollection ILookup IFn IObj IMeta Associative MapEquivalence Seqable]))

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
  IPersistentCollection
    (cons [this [k v]] (assoc this k v))
    (equiv [this o]
           (or (and (isa? (class o) SetMap)
                    (= contents (.contents ^SetMap o)))
               (= contents o)))
    (empty [this] (SetMap. metadata (empty contents)))
    (count [this] (count contents))
  IPersistentSet
    (disjoin [this [k v]]
             (if-let [old-v-set (get contents k)]
                     (SetMap. metadata
                              (if (< 1 (count old-v-set))
                                  (assoc contents k (disj old-v-set v))
                                  (dissoc contents k)))
                     this))
  IObj (withMeta [this new-meta] (SetMap. new-meta contents))
  ; Boilerplate map-like object implementation code. Common to all the mirrored maps, and also to SetMap (although SetMap uses differing field names).
  Associative
    (containsKey [this k] (contains? contents k))
    (entryAt     [this k] (find contents k))
  ILookup
    (valAt [this k]           (get contents k))
    (valAt [this k not-found] (get contents k not-found))
  Seqable (seq      [this]   (seq contents))
  IFn     (invoke   [this k] (get contents k))
  IMeta   (meta     [this]   metadata)
  Object  (toString [this]   (str contents))
  MapEquivalence)

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
    (cons [this [k v]] (assoc this k v))
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
  ; Boilerplate map-like object implementation code. Common to all the mirrored maps, and also to SetMap (although SetMap uses differing field names).
  Associative
    (containsKey [this k] (contains? active k))
    (entryAt     [this k] (find active k))
  ILookup
    (valAt [this k]           (get active k))
    (valAt [this k not-found] (get active k not-found))
  Seqable (seq      [this]   (seq active))
  IFn     (invoke   [this k] (get active k))
  IMeta   (meta     [this]   metadata)
  Object  (toString [this]   (str active))
  MapEquivalence)

; Dual (invertible) SetMap with no restrictions on associations; that is to say, a bipartite graph.
(deftype- Bipartite [metadata
                    ^SetMap active
                    ^SetMap mirror]
  Invertible (inverse [this] (Bipartite. metadata mirror active))
  IPersistentMap
    (assoc [this k v]
           (Bipartite. metadata (assoc active k v) (assoc mirror v k)))
    (without [this k]
             (reduce disj this (map (partial vector k) (get active k))))
  IPersistentCollection
    (cons [this [k v]] (assoc this k v))
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
  ; Boilerplate map-like object implementation code. Common to all the mirrored maps, and also to SetMap (although SetMap uses differing field names).
  Associative
    (containsKey [this k] (contains? active k))
    (entryAt     [this k] (find active k))
  ILookup
    (valAt [this k]           (get active k))
    (valAt [this k not-found] (get active k not-found))
  Seqable (seq      [this]   (seq active))
  IFn     (invoke   [this k] (get active k))
  IMeta   (meta     [this]   metadata)
  Object  (toString [this]   (str active))
  MapEquivalence)

; Drop-in replacement for normal associative map, with the additional functionality of invertibility. Yields an InvertedSurjection when inverted.
(deftype- Surjection [metadata
                     ^IPersistentMap active
                     ^SetMap mirror]
  Invertible (inverse [this] (inverted-surjection- metadata mirror active))
  IPersistentMap
    (assoc [this k v]
           (Surjection. metadata
                        (assoc active k v)
                        (assoc (disj mirror [(get active k) k]) v k)))
    (without [this k] 
             (disj this [k (get active k)]))
  IPersistentCollection
    (cons [this [k v]] (assoc this k v))
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
  ; Boilerplate map-like object implementation code. Common to all the mirrored maps, and also to SetMap (although SetMap uses differing field names).
  Associative
    (containsKey [this k] (contains? active k))
    (entryAt     [this k] (find active k))
  ILookup
    (valAt [this k]           (get active k))
    (valAt [this k not-found] (get active k not-found))
  Seqable (seq      [this]   (seq active))
  IFn     (invoke   [this k] (get active k))
  IMeta   (meta     [this]   metadata)
  Object  (toString [this]   (str active))
  MapEquivalence)

; Dual of Surjection. Behaves like a SetMap, except it preserves the surjective property of the original map. Yields a Surjection when inverted.
(deftype- InvertedSurjection [metadata
                             ^SetMap active
                             ^IPersistentMap mirror]
  Invertible (inverse [this] (surjection- metadata mirror active))
  IPersistentMap
    (assoc [this k v]
           (InvertedSurjection. metadata
                                (assoc (disj active [(get mirror v) v]) k v)
                                (assoc mirror v k)))
    (without [this k] 
             (reduce disj this (map (partial vector k) (get active k))))
  IPersistentCollection
    (cons [this [k v]] (assoc this k v))
    (equiv [this o]
           (or (and (isa? (class o) InvertedSurjection)
                    (= active (.active ^InvertedSurjection o)))
               (= active o)))
    (count [this] (count active))
    (empty [this] (InvertedSurjection. metadata (empty active) (empty mirror)))
  IPersistentSet
    (disjoin [this [k v]]
             (if (and (contains? active k) (contains? mirror v))
                 (InvertedSurjection. metadata (disj active [k v]) (dissoc mirror v))
                 this))
  IObj (withMeta [this new-meta] (InvertedSurjection. new-meta active mirror))
  ; Boilerplate map-like object implementation code. Common to all the mirrored maps, and also to SetMap (although SetMap uses differing field names).
  Associative
    (containsKey [this k] (contains? active k))
    (entryAt     [this k] (find active k))
  ILookup
    (valAt [this k]           (get active k))
    (valAt [this k not-found] (get active k not-found))
  Seqable (seq      [this]   (seq active))
  IFn     (invoke   [this k] (get active k))
  IMeta   (meta     [this]   metadata)
  Object  (toString [this]   (str active))
  MapEquivalence)

; Private factory functions for InvertedSurjection and Surjection. Required because of limitations on coroutined type definitions.
(defn- inverted-surjection-
  ([metadata active mirror] (InvertedSurjection. metadata active mirror)))
(defn- surjection-
  ([metadata active mirror] (Surjection. metadata active mirror)))

; Factory functions for the core datatypes in this file:

(defn set-map
  "Creates a SetMap, which is a smart map that overrides assoc so that every value is a set of all values which have been associated with it; that is, assoc is non-overwriting."
  ([] (SetMap. nil (hash-map)))
  ([& keyvals]
   (apply assoc (set-map) keyvals)))

(defn bijection
  "Creates a Bijection, which is an invertible map that preserves a bijective (1-to-1) mapping. That is, both keys and values are guaranteed to be unique; assoc overwrites any extant keys or values, also removing their associated pairings."
  ([] (Bijection. nil (hash-map) (hash-map)))
  ([& keyvals]
   (apply assoc (bijection) keyvals)))

(defn surjection
  "Creates a Surjection, which is an invertible map which functions as a drop-in replacement for the standard map. Inverts into an InvertedSurjection, which behaves like a SetMap that is constrained to preserving the surjective property of the original map -- and which may be inverted back into a Surjection."
  ([] (Surjection. nil (hash-map) (set-map)))
  ([& keyvals]
   (apply assoc (surjection) keyvals)))

(defn bipartite
  "Creates a Bipartite, which is an invertible map which maintains a mapping from keys to sets of values, and values to sets of keys -- that is, essentially an invertible SetMap. So named because it is a bipartite graph in semantic structure."
  ([] (Bipartite. nil (set-map) (set-map)))
  ([& keyvals]
   (apply assoc (bipartite) keyvals)))

(defn rdissoc
  "Dissociates every key mapped to any value in vs. Works only with things implementing the Invertible protocol."
  ([coll & vs]
   (inverse (apply dissoc (inverse coll) vs))))

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
