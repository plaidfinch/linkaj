(ns map-experiments.smart-maps.surjection
  (:require [map-experiments.smart-maps.protocol :refer :all]
            [map-experiments.smart-maps.set-map  :refer :all])
  (:import [clojure.lang
            IPersistentMap IPersistentSet IPersistentCollection IEditableCollection ITransientMap ITransientSet ILookup IFn IObj IMeta Associative MapEquivalence Seqable MapEntry SeqIterator]
           [map_experiments.smart_maps.set_map SetMap TransientSetMap]))

; Forward declaration of coroutined constructors for surjection types.
(declare inverted-surjection- surjection- transient-surjection- transient-inverted-surjection-)

; Drop-in replacement for normal associative map, with the additional functionality of invertibility. Yields an InvertedSurjection when inverted.
(deftype Surjection [metadata
                     ^IPersistentMap active
                     ^SetMap mirror]
  Invertible (inverse [this] (inverted-surjection- metadata mirror active))
  ISmartMap (plain [this] active)
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
    (SeqIterator. (seq this)))
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
  IEditableCollection
  (asTransient [this] (transient-surjection- (transient active) (transient mirror)))
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

(deftype TransientSurjection [^{:unsynchronized-mutable true} active
                              ^{:unsynchronized-mutable true
                                :tag TransientSetMap} mirror]
  Invertible
  (inverse [this]
           (transient-inverted-surjection- mirror active))
  TransientInvertible
  (inverse! [this]
            (transient-inverted-surjection- mirror active))
  ITransientMap
  (count [this] (count active))
  (valAt [this k] (get active k))
  (valAt [this k not-found] (get active k not-found))
  (assoc [this k v]
         (set! active (assoc! active k v))
         (let [old-v (get active k ::not-found)]
              (if (not= ::not-found old-v)
                  (set! mirror (assoc! (disj! mirror [old-v k]) v k))))
         this)
  (conj [this x]
        (if (and (sequential? x) (= 2 (count x)))
            (let [[k v] x]
                 (.assoc ^TransientSurjection this k v))
            (throw (IllegalArgumentException.
                     "Vector arg to map conj must be a pair"))))
  (without [this k]
           (disj! this [k (get active k)]))
  (persistent [this]
              (Surjection. nil (persistent! active) (persistent! mirror)))
  ITransientSet
  (disjoin [this [k v]]
           (if (not (some #(= ::not-found %)
                          [(get active k ::not-found) (get mirror v ::not-found)]))
               (do (set! active (dissoc! active k))
                   (set! mirror (disj! mirror [v k]))))
           this))

; Dual of Surjection. Behaves like a SetMap, except it preserves the surjective property of the original map. Yields a Surjection when inverted.
(deftype InvertedSurjection [metadata
                             ^SetMap active
                             ^IPersistentMap mirror]
  Invertible (inverse [this] (surjection- metadata mirror active))
  ISmartMap (plain [this] (plain active))
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
  IEditableCollection
  (asTransient [this] (transient-inverted-surjection- (transient active) (transient mirror)))
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

(deftype TransientInvertedSurjection [^{:unsynchronized-mutable true
                                        :tag TransientSetMap} active
                                      ^{:unsynchronized-mutable true} mirror]
  Invertible
  (inverse [this]
           (transient-surjection- mirror active))
  TransientInvertible
  (inverse! [this]
            (transient-surjection- mirror active))
  ITransientMap
  (count [this] (count active))
  (valAt [this k] (get active k))
  (valAt [this k not-found] (get active k not-found))
  (assoc [this k v]
         (let [old-k (get mirror v ::not-found)]
              (if (not= ::not-found old-k)
                  (set! active (assoc! (disj! active [old-k v]) k v))))
         (set! mirror (assoc! mirror v k))
         this)
  (conj [this x]
        (if (and (sequential? x) (= 2 (count x)))
            (let [[k v] x]
                 (assoc! this k v))
            (throw (IllegalArgumentException.
                     "Vector arg to map conj must be a pair"))))
  (without [this k]
           (disj! this [k (get active k)]))
  (persistent [this]
              (InvertedSurjection. nil (persistent! active) (persistent! mirror)))
  ITransientSet
  (disjoin [this [k v]]
           (if (not (some #(= ::not-found %)
                          [(get active k ::not-found) (get mirror v ::not-found)]))
               (do (set! active (disj! active [k v]))
                   (set! mirror (dissoc! mirror v))))
           this))

; Private factory functions for InvertedSurjection and Surjection. Required because of limitations on coroutined type definitions.
(defn- inverted-surjection-
  ([metadata active mirror] (InvertedSurjection. metadata active mirror)))
(defn- surjection-
  ([metadata active mirror] (Surjection. metadata active mirror)))

; Factory functions for transient surjections and inverted surjections.
(defn transient-surjection-
  ([active mirror] (TransientSurjection. active mirror)))
(defn transient-inverted-surjection-
  ([active mirror] (TransientInvertedSurjection. active mirror)))

(defn surjection
  "Creates a Surjection, which is an invertible map which functions as a drop-in replacement for the standard map. Inverts into an InvertedSurjection, which behaves like a SetMap that is constrained to preserving the surjective property of the original map -- and which may be inverted back into a Surjection."
  ([] (Surjection. nil (hash-map) (set-map)))
  ([& keyvals]
   (apply assoc (surjection) keyvals)))
