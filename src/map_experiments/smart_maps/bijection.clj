(ns map-experiments.smart-maps.bijection
  (:require [map-experiments.smart-maps.protocol :refer :all])
  (:import [clojure.lang
            IPersistentMap IPersistentSet IPersistentCollection IEditableCollection ITransientMap ITransientSet ILookup IFn IObj IMeta Associative MapEquivalence Seqable MapEntry SeqIterator]))

(declare transient-bijection-)

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
  IEditableCollection
  (asTransient [this] (transient-bijection- (transient active) (transient mirror)))
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

(deftype TransientBijection [^{:unsynchronized-mutable true} active
                             ^{:unsynchronized-mutable true} mirror]
  Invertible
  (inverse [this]
           (transient-bijection- mirror active))
  TransientInvertible
  (inverse! [this]
            (let [a active
                  m mirror]
                 (set! active m)
                 (set! mirror a)
                 this))
  ITransientMap
  (count [this] (count active))
  (valAt [this k] (get active k))
  (valAt [this k not-found] (get active k not-found))
  (assoc [this k v]
         (set! active (assoc! (dissoc! active (get mirror v)) k v))
         (set! mirror (assoc! (dissoc! mirror (get active k)) v k))
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
              (Bijection. nil (persistent! active) (persistent! mirror)))
  ITransientSet
  (disjoin [this [k v]]
           (if (not (some #(= ::not-found %)
                          [(get active k ::not-found) (get mirror v ::not-found)]))
               (do (set! active (dissoc! active k))
                   (set! mirror (dissoc! mirror v))))
           this))

(defn- transient-bijection- [active mirror]
  (TransientBijection. active mirror))

(defn bijection
  "Creates a Bijection, which is an invertible map that preserves a bijective (1-to-1) mapping. That is, both keys and values are guaranteed to be unique; assoc overwrites any extant keys or values, also removing their associated pairings."
  ([] (Bijection. nil (hash-map) (hash-map)))
  ([& keyvals]
   (apply assoc (bijection) keyvals)))
