(ns map-experiments.smart-maps.attr-map
  (:require [map-experiments.common :refer :all]
            [map-experiments.smart-maps.protocol :refer :all]
            [map-experiments.smart-maps.bipartite :refer :all]
            [map-experiments.smart-maps.surjection :refer :all]
            [clojure.set :as set])
  (:import [clojure.lang
            IPersistentMap IPersistentSet IPersistentCollection IEditableCollection ITransientMap ITransientSet ILookup IFn IObj IMeta Associative MapEquivalence Seqable MapEntry SeqIterator]
           [map_experiments.smart_maps.set_map SetMap]))

; An AttributeMap is a mapping from keys to attribute-value pairs.
; It presents itself as a map where values are maps, but is also optimized for fast queries about which keys have particular attributes. The underlying implementation is *not* the same as the view presented by toString and print-method; rather, an AttributeMap is internally a bijection between keys and attributes they have, as well as a map where values are attributes and keys are surjections from keys to values (for that attribute).
(deftype AttributeMap [metadata keys-attrs contents]
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
  (attr-rename [this old-attr new-attr]
               (AttributeMap.
                 metadata
                 (inverse
                   (into (dissoc (inverse keys-attrs) old-attr)
                         (map (partial vector new-attr)
                              (get (inverse keys-attrs) old-attr))))
                 (assoc (dissoc contents old-attr) new-attr (get contents old-attr))))
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
  (iterator [this]
    (SeqIterator. (seq this)))
  IPersistentCollection
  (cons [this x]
        (cond (and (sequential? x) (= 3 (count x)))
              (let [[k a v] x] (attr-assoc this k a v))
              (instance? clojure.lang.MapEntry x)
              (let [[k a-v-map] x] (assoc this k a-v-map))
              (map? x)
              (reduce (partial apply assoc) this x)
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
                              (MapEntry. k (get this k))))
  ILookup
  (valAt [this k]
         (let [result
               (into {} (map (comp (juxt key #(get (val %) k))
                                   (partial find contents))
                             (get keys-attrs k)))]
              (when (seq result) result)))
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

(defn attr-map
  "Creates an AttributeMap, which is a map where all values are maps between attributes and values. Supports fast lookup of keys based on attributes and values."
  ([] (AttributeMap. nil (bipartite) (hash-map)))
  ([& keyvals]
   (apply assoc (attr-map) keyvals)))

(defn keys-with-all
  "Returns all keys which match every attribute-value pair specified in the map."
  ([m a-v-map]
   (apply set/intersection (map (partial apply keys-with m) a-v-map))))

(def specific-key
  "Returns nil if no keys match, a key if one key matches, or an error if more than one key matches the specification. Designed to be used when it is known that particular types of queries are guaranteed to be unique."
  (specific keys-with-all))
