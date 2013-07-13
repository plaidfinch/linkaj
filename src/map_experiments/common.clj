(ns map-experiments.common
  (:require [map-experiments.smart-maps.protocol :refer :all])
  (:import [clojure.lang
            IPersistentMap IPersistentSet]))

; Functions used by many of the files...

(prefer-method print-method
  IPersistentMap IPersistentSet)

(defn transientize
  "If x is nil or of the same type as empty-value, returns a transient version of x. Otherwise, (e.g. if x is already transient) returns x."
  ([empty-value x]
   (cond (nil? x) (transient empty-value)
         (instance? (type empty-value) x) (transient x)
         :else x)))

(defn persistentize!
  "If x is nil or of the same type as empty-value, returns a persistent version of x. Otherwise, (e.g. if x is already persistent) returns x."
  ([empty-value x]
   (cond (nil? x) (persistent! empty-value)
         (instance? (type empty-value) x) (persistent! x)
         :else x)))

(defn setify
  "Ensures that x is a set, but does not do extra work if it already is."
  ([x] (cond (set? x) x
             (sequential? x) (set x)
             :else #{x})))

(defn sequentialize
  "Ensures that x is a sequence (by putting it in a 1-length list if not)."
  ([x] (if (sequential? x) (seq x) (seq (list x)))))

(defn rdissoc
  "Dissociates every key mapped to any value in vs. Works only with things implementing the Invertible protocol."
  ([coll & vs]
   (inverse (apply dissoc (inverse coll) vs))))

(defn rdissoc!
  "Destructively dissociates every key mapped to any value in vs. Works only with things implementing the TransientInvertible protocol."
  ([coll & vs]
   (inverse! (apply dissoc! (inverse! coll) vs))))

(defn opposite
  "Returns the opposite value of x in the given bijection (whichever side the opposite is on) and nil if neither side contains the item, or not-found if specified."
  ([bij x]
   (opposite bij x nil))
  ([bij x not-found]
   (if-let [result (or (find bij x) (find (inverse bij) x))]
           (val result)
           not-found)))

(defn map-cross
  "Takes a map where keys are sequences are returns a sequence of maps where keys are every possible pick of a key for each value (cartesian product analogue for maps of sequences). If any key is not a sequence, it is treated as if it is a sequence of one item."
  ([m] (when-let [[ks vs] (seq (first m))]
                 (if-let [rest-maps (map-cross (dissoc m ks))]
                         (for [k (sequentialize ks)
                               v (sequentialize vs)
                               r rest-maps]
                              (assoc r k v))
                         (for [k (sequentialize ks)
                               v (sequentialize vs)]
                              {k v})))))

(defn specific
  "Wraps a function returning a collection so that it will return nil for an empty collection, the single element contained for a singleton collection, and will throw an error for a collection with more than one element."
  ([function]
   (fn [& args]
     (when-let [result (apply function args)]
               (if (not (seq (rest result)))
                   (first result)
                   (throw (IllegalArgumentException.
                            (str "Violation of 'specific' constraint on function <" function ">. The function, when given argument(s) " args ", returned a collection with more than one element" #_": " #_(with-out-str (pr result)) "."))))))))

