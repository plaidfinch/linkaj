(ns map-experiments.smart-maps.surjection
  (:require [map-experiments.smart-maps.protocol :refer :all]
            [map-experiments.smart-maps.set-map :refer :all])
  (:import [clojure.lang
            IPersistentMap IPersistentSet IPersistentCollection IEditableCollection ITransientMap ITransientSet ILookup IFn IObj IMeta Associative MapEquivalence Seqable]
           [map_experiments.smart_maps.set_map SetMap]))

; Forward declaration of coroutined constructors for surjection types.
(declare inverted-surjection- surjection-)

; Drop-in replacement for normal associative map, with the additional functionality of invertibility. Yields an InvertedSurjection when inverted.
(deftype Surjection [metadata
                     ^IPersistentMap active
                     ^SetMap mirror]
  Invertible (inverse [this] (inverted-surjection- metadata mirror active))
  IPersistentMap
  (assoc [this k v]
         (Surjection. metadata
                      (assoc active k v)
                      (assoc
                        (if-let [[_ old-v] (find active k)]
                                (disj mirror [old-v k])
                                mirror)
                        v k)))
  (without [this k] 
           (disj this [k (get active k)]))
  (iterator [this]
    (clojure.lang.SeqIterator. (seq this)))
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
(deftype InvertedSurjection [metadata
                             ^SetMap active
                             ^IPersistentMap mirror]
  Invertible (inverse [this] (surjection- metadata mirror active))
  IPersistentMap
  (assoc [this k v]
         (InvertedSurjection. metadata
                              (assoc
                                (if-let [[_ old-k] (find mirror v)]
                                        (disj active [old-k v])
                                        active)
                                k v)
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

(defn surjection
  "Creates a Surjection, which is an invertible map which functions as a drop-in replacement for the standard map. Inverts into an InvertedSurjection, which behaves like a SetMap that is constrained to preserving the surjective property of the original map -- and which may be inverted back into a Surjection."
  ([] (Surjection. nil (hash-map) (set-map)))
  ([& keyvals]
   (apply assoc (surjection) keyvals)))
