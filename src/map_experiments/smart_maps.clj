(ns map-experiments.smart-maps
  (:require [clojure.set :as set])
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

; Extend the Invertible protocol to nil, for reasons of internal use.
(extend-protocol Invertible nil (inverse [m] nil))

(defprotocol IAttributeMap
  "Protocol for a map from keys to attribute-value pairs."
  (keys-with [m a v]
    "Returns all keys with attribute a associated with value v.")
  (keys-with-attr [m a]
    "Returns all keys with attribute a.")
  (attr-get [m k a] [m k a not-found]
    "Returns the value associated with attribute a for key k. Returns nil or not-found if there is no such value.")
  (attr-assoc [m k a v]
    "Associates attribute a with value v for key k.")
  (attr-dissoc [m k a]
    "Dissociates attribute a from key k.")
  (attr-remove [m a]
    "Removes all instances of attribute a from the map."))

; Always default to mappy printing for things which are both mappy and setty.
(prefer-method
  print-method
  IPersistentMap
  IPersistentSet)

; Forward declaration of coroutined constructors for surjection types.
(declare inverted-surjection- surjection-)

; Forward declaration of all the factory functions for the types in this file.
(declare set-map surjection bijection bipartite attr-map)

; I use rdissoc before it is defined, in the AttributeMap definition.
(declare rdissoc)

; Here begins the meat of the file: the type definitions for the various classes contained herein. Ordering is as follows:

; SetMap
; Bijection
; Bipartite
; Surjection
; InvertedSurjection
; AttributeMap

; All types are defined as private because their naked constructors DO NOT GUARANTEE VALID INTERNAL STATE. Do not use the naked constructors for any of these types; instead, you should use the factory functions listed near the bottom of this file.

; A SetMap is like a regular map, but forces keys to be sets, and overrides assoc so that it augments the set at that key rather than replacing the value. It's used as a building block for the later constructs.
(deftype- SetMap [metadata contents]
  IPersistentMap
    (assoc [this k v]
           (SetMap. metadata (assoc contents k ((fnil conj #{}) (get contents k) v))))
    (without [this k]
             (SetMap. metadata (dissoc contents k)))
  IPersistentCollection
    (cons [this x]
          (if (and (sequential? x) (= 2 (count x)))
              (let [[k v] x]
                (assoc this k v))
            (throw (IllegalArgumentException.
                     "Vector arg to map conj must be a pair"))))
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
    (cons [this x]
          (if (and (sequential? x) (= 2 (count x)))
              (let [[k v] x]
                (assoc this k v))
            (throw (IllegalArgumentException.
                     "Vector arg to map conj must be a pair"))))
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
    (cons [this x]
          (if (and (sequential? x) (= 2 (count x)))
              (let [[k v] x]
                (assoc this k v))
            (throw (IllegalArgumentException.
                     "Vector arg to map conj must be a pair"))))
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
    (cons [this x]
          (if (and (sequential? x) (= 2 (count x)))
              (let [[k v] x]
                (assoc this k v))
            (throw (IllegalArgumentException.
                     "Vector arg to map conj must be a pair"))))
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
    (cons [this x]
          (if (and (sequential? x) (= 2 (count x)))
              (let [[k v] x]
                (assoc this k v))
              (throw (IllegalArgumentException.
                       "Vector arg to map conj must be a pair"))))
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

; An AttributeMap is a mapping from keys to attribute-value pairs.
; It presents itself as a map where values are maps, but is also optimized for fast queries about which keys have particular attributes. The underlying implementation is *not* the same as the view presented by toString and print-method; rather, an AttributeMap is internally a bijection between keys and attributes they have, as well as a map where values are attributes and keys are surjections from keys to values (for that attribute).
(deftype- AttributeMap [metadata keys-attrs contents]
  IAttributeMap
    (keys-with [this a v]
               (get (inverse (get contents a)) v))
    (keys-with-attr [this a]
                    (get (inverse keys-attrs) a))
    (attr-get [this k a]
              (get (get contents a) k))
    (attr-get [this k a not-found]
              (get (get contents a) k not-found))
    (attr-assoc [this k a v]
                (AttributeMap.
                  metadata
                  (assoc keys-attrs k a)
                  (assoc contents a ((fnil assoc (surjection)) (get contents a) k v))))
    (attr-dissoc [this k a]
                 (if-let [old-v-map (get contents a)]
                   (AttributeMap.
                     metadata
                     (disj keys-attrs [k a])
                     (if (< 1 (count old-v-map))
                         (assoc contents a (dissoc old-v-map k))
                         (dissoc contents a)))
                   this))
    (attr-remove [this a]
                 (AttributeMap.
                   metadata
                   (rdissoc keys-attrs a)
                   (dissoc contents a)))
  IPersistentMap
    (assoc [this k a-v-map]
           (try
             (reduce (partial apply attr-assoc) this (map (partial cons k) a-v-map))
             (catch Exception e
               (throw (IllegalArgumentException.
                        "Value argument to AttributeMap assoc must be a map of attributes and values, or a sequence which can be converted into such.")))))
    (without [this k]
             (AttributeMap.
               metadata
               (dissoc keys-attrs k)
               (reduce #(assoc %1 %2 (dissoc (get %1 %2) k))
                       contents
                       (get keys-attrs k))))
  IPersistentCollection
    (cons [this x]
          (cond (instance? clojure.lang.MapEntry x)
                  (let [[k a-v-map] x] (assoc this k a-v-map))
                (map? x)
                  (reduce (partial apply assoc) this x)
                (and (sequential? x) (= 3 (count x)))
                  (let [[k a v] x] (attr-assoc this k a v))
                :else
                  (throw (IllegalArgumentException.
                           "Argument to AttributeMap conj must be a map, a map entry, or a 3-tuple."))))
    (equiv [this o]
           (and (isa? (class o) AttributeMap)
                (= contents (.contents ^AttributeMap o))))
    (empty [this] (AttributeMap. metadata (empty keys-attrs) (empty contents)))
    (count [this] (count keys-attrs))
  Associative
    (containsKey [this k] (contains? keys-attrs k))
    (entryAt     [this k] (when (contains? this k)
                                (clojure.lang.MapEntry. k (get this k))))
  ILookup
    (valAt [this k]
           (into {} (map (comp (juxt key #(get (val %) k))
                               (partial find contents))
                         (get keys-attrs k))))
    (valAt [this k not-found]
           (if (contains? this k)
               (get this k)
               not-found))
  Seqable (seq      [this]   (map (partial find this) (keys keys-attrs)))
  IFn     (invoke   [this k] (get this k))
  IMeta   (meta     [this]   metadata)
  IObj    (withMeta [this new-meta] (AttributeMap. new-meta keys-attrs contents))
  Object  (toString [this]   (str (into {} (seq this))))
  MapEquivalence)

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

(defn attr-map
  "Creates an AttributeMap, which is a map where all values are maps between attributes and values. Supports fast lookup of keys based on attributes and values."
  ([] (AttributeMap. nil (bipartite) (hash-map)))
  ([& keyvals]
   (apply assoc (attr-map) keyvals)))

; Some other functions for use with some of the datatypes:

(defn rdissoc
  "Dissociates every key mapped to any value in vs. Works only with things implementing the Invertible protocol."
  ([coll & vs]
   (inverse (apply dissoc (inverse coll) vs))))

(defn keys-with-all
  "Returns all keys which match every attribute-value pair specified in the map."
  ([m a-v-map]
   (apply set/intersection (map (partial apply keys-with m) a-v-map))))

(defn specific-key
  "Returns nil if no keys match, a key if one key matches, or an error if more than one key matches the specification. Designed to be used when it is known that particular types of queries are guaranteed to be unique."
  ([m a-v-map]
   (when-let [s (keys-with-all m a-v-map)]
     (if (= 1 (count s))
         (first s)
         (throw (IllegalArgumentException.
                  (str "The " (count s) " keys " s " each have have all the attribute(s) " a-v-map "; more specificity is required to match only a single key.")))))))
