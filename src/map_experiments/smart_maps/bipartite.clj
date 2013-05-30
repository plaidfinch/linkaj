(ns map-experiments.smart-maps.bipartite
  (:require [map-experiments.smart-maps.protocol :refer :all]
            [map-experiments.smart-maps.set-map :refer :all])
  (:import [clojure.lang
            IPersistentMap IPersistentSet IPersistentCollection IEditableCollection ITransientMap ITransientSet ILookup IFn IObj IMeta Associative MapEquivalence Seqable MapEntry SeqIterator]
           [map_experiments.smart_maps.set_map SetMap]))

; Dual (invertible) SetMap with no restrictions on associations; that is to say, a bipartite graph.
(deftype Bipartite [metadata
                    ^SetMap active
                    ^SetMap mirror]
  Invertible (inverse [this] (Bipartite. metadata mirror active))
  IPersistentMap
  (assoc [this k v]
         (Bipartite. metadata (assoc active k v) (assoc mirror v k)))
  (without [this k]
           (reduce disj this (map (partial vector k) (get active k))))
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

(defn bipartite
  "Creates a Bipartite, which is an invertible map which maintains a mapping from keys to sets of values, and values to sets of keys -- that is, essentially an invertible SetMap. So named because it is a bipartite graph in semantic structure."
  ([] (Bipartite. nil (set-map) (set-map)))
  ([& keyvals]
   (apply assoc (bipartite) keyvals)))
