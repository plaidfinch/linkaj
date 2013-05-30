(ns map-experiments.smart-maps.protocol)

(defprotocol Invertible
  "Protocol for a map which can be inverted, preferably in O(1) time."
  (inverse [m] "Returns an invertible map inverted."))

(defprotocol TransientInvertible
  "Protocol for a transient invertible map."
  (inverse! [m] "Returns the map inverted, destructively."))

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
               "Removes all instances of attribute a from the map.")
  (attr-rename [m old-attr new-attr]
               "Renames old-attr to new-attr in the map."))
