(ns map-experiments.directed-graph.core
  (:require [map-experiments.directed-graph.protocol :refer :all]
            [map-experiments.smart-maps              :refer :all]
            [clojure.set                             :refer :all]))

(defn opposite
  "Returns the opposite value of x in the given bijection (whichever side the opposite is on) and nil if neither side contains the item, or not-found if specified."
  ([bij x]
   (opposite bij x nil))
  ([bij x not-found]
   (if-let [result (or (find bij x) (find (inverse bij) x))]
           (val result)
           not-found)))

(defn key-overlap?
  "Returns true if two maps have any shared keys."
  ([& ms]
   (not (nil? (seq (apply intersection (map (comp set keys) ms)))))))

(defn map-cross
  "Takes a map where keys are sequences are returns a sequence of maps where keys are every possible pick of a key for each value (cartesian product analogue for maps of sequences)."
  ([m]
   (when (seq m)
         (if-let [[k vs] (first m)]
                 (if (seq (dissoc m k))
                     (for [v vs
                           r (map-cross (dissoc m k))]
                          ((fnil conj {}) r [k v]))
                     (for [v vs]
                          (hash-map k v)))))))

; Private functions for internal things:

(defn parse-relations
  ([attributes relations-map]
   (let [relations
         (select-keys attributes
                      (concat (keys relations-map)
                              (keys (inverse relations-map))))
         rest-attrs (apply dissoc attributes (keys relations))]
        [relations rest-attrs])))

(deftype DirectedGraph [nodes-map
                        edges-map
                        node-id-seq
                        edge-id-seq
                        relations-map
                        constraints-map
                        metadata]
  
  IDirectedGraph
  ; Methods acting on nodes:
  (nodes [this]
         (set (keys nodes-map)))
  (nodes [this query]
         (if (seq query)
             (apply intersection
                    (for [[a vs] query]
                         (apply union (for [v vs] (keys-with nodes-map a v)))))
             (nodes this)))
  (node? [this o] (contains? nodes-map o))
  (get-node [this node-key] (get nodes-map node-key))
  (add-node [this attributes]
            (if (or (key-overlap? attributes relations-map)
                    (key-overlap? attributes (inverse relations-map)))
                (throw (IllegalArgumentException.
                         "Attributes may not be identical to existing relations"))
                (DirectedGraph.
                  (assoc nodes-map (first node-id-seq) attributes)
                  edges-map
                  (rest node-id-seq)
                  edge-id-seq
                  relations-map
                  constraints-map
                  metadata)))
  (remove-node [this node-key]
               (assert false "THIS METHOD NEEDS TO BE FIXED: REMOVE CONNECTED EDGES")
               (DirectedGraph.
                 (dissoc nodes-map node-key)
                 edges-map
                 (if (contains? nodes-map node-key)
                     (cons node-key node-id-seq)
                     node-id-seq)
                 edge-id-seq
                 relations-map
                 constraints-map
                 metadata))
  (assoc-node [this node-key attributes]
              (if (or (key-overlap? attributes relations-map)
                      (key-overlap? attributes (inverse relations-map)))
                  (throw (IllegalArgumentException.
                           "Attributes may not be identical to existing relations"))
                  (if (not (contains? nodes-map node-key))
                      (throw (IllegalArgumentException.
                               "Node must exist before assoc-ing onto it; to create a new node with attributes, use add-node"))
                      (DirectedGraph.
                        (assoc nodes-map node-key attributes)
                        edges-map
                        node-id-seq
                        edge-id-seq
                        relations-map
                        constraints-map
                        metadata))))
  (dissoc-node [this node-key attribute-keys]
               (let [new-nodes-map (reduce #(attr-dissoc %1 node-key %2)
                                           nodes-map attribute-keys)]
                    (if (not= (count new-nodes-map) (count nodes-map))
                        (throw (IllegalArgumentException.
                                 "At least one attribute must be attached to each node for it to exist (and for it to be useful); thus, you can't remove every attribute from a node"))
                        (DirectedGraph.
                          new-nodes-map
                          edges-map
                          node-id-seq
                          edge-id-seq
                          relations-map
                          constraints-map
                          metadata))))
  ; Methods acting on edges:
  (edges [this]
         (set (keys edges-map)))
  (edges [this query]
         (if (seq query)
             (apply intersection
                    (for [[a vs] query]
                         (apply union (for [v vs] (keys-with edges-map a v)))))
             (edges this)))
  (edge? [this o] (contains? edges-map o))
  (get-edge [this edge-key] (get edges-map edge-key))
  (add-edge [this attributes]
            (let [[relations rest-attrs] (parse-relations attributes relations-map)]
                 (if (not= 2 (count relations))
                     (throw (IllegalArgumentException.
                              "An edge must have relations to exactly two nodes"))
                     (let [[r1 r2] (keys relations)]
                          (if (not (= r1 (opposite relations-map r2)))
                              (throw (IllegalArgumentException.
                                       "Relations given for an edge must be opposites"))
                              (if (not (and (contains? nodes-map (relations r1))
                                            (contains? nodes-map (relations r2))))
                                  (throw (IllegalArgumentException.
                                           "An edge must connect to existing nodes"))
                                  (DirectedGraph.
                                    nodes-map
                                    (assoc edges-map (first edge-id-seq) attributes)
                                    node-id-seq
                                    (rest edge-id-seq)
                                    relations-map
                                    constraints-map
                                    metadata)))))))
  (remove-edge [this edge-key]
               (DirectedGraph.
                 nodes-map
                 (dissoc edges-map edge-key)
                 node-id-seq
                 (if (contains? edges-map edge-key)
                     (cons edge-key edge-id-seq)
                     edge-id-seq)
                 relations-map
                 constraints-map
                 metadata))
  
  
  Relational
  (relations [this] relations-map)
  (related-in? [this r1 r2]
               (and (or (and (contains? relations-map r1)
                             (contains? (inverse relations-map) r2))
                        (and (contains? (inverse relations-map) r1)
                             (contains? relations-map r2)))
                    (= r1 (opposite relations-map r2))))
  (add-relation [this r1 r2]
                (DirectedGraph.
                  nodes-map
                  edges-map
                  node-id-seq
                  edge-id-seq
                  (assoc relations-map r1 r2)
                  constraints-map
                  metadata))
  (remove-relation [this r1 r2]
                   (if (related-in? this r1 r2)
                       (DirectedGraph.
                         nodes-map
                         edges-map
                         node-id-seq
                         edge-id-seq
                         (dissoc (rdissoc relations-map r1) r1)
                         constraints-map
                         metadata)
                       (throw (IllegalArgumentException.
                                "One or both of the relations specified is not present in the object or is not related to the other relation given."))))
  
  Constrained
  (constraints [this] constraints-map)
  (add-constraint [this k f]
                  (do (assert (not (contains? constraints-map k))
                              (str "Object already has a constraint with key " k))
                      (DirectedGraph.
                        nodes-map
                        edges-map
                        node-id-seq
                        edge-id-seq
                        relations-map
                        (assoc constraints-map k f)
                        metadata)))
  (remove-constraint [this k]
                     (DirectedGraph.
                       nodes-map
                       edges-map
                       node-id-seq
                       edge-id-seq
                       relations-map
                       (dissoc constraints-map k)
                       metadata))
  (assert-constraints [this] "NOT YET IMPLEMENTED"))

(defn digraph []
  (DirectedGraph.
    (attr-map)
    (attr-map)
    (map (comp keyword (partial str "N-")) (iterate inc 0))
    (map (comp keyword (partial str "E-")) (iterate inc 0))
    (bijection)
    (hash-map)
    (hash-map)))

(def node
  "For selecting a single node when you know the query is unique."
  (specific nodes))

(def edge
  "For selecting a single edge when you know the query is unique."
  (specific edges))

(defn get-nodes
  "Gets the values of all node keys given from the graph."
  ([graph node-keys]
   (map (partial get-node graph) (nodes graph))))

(defn add-nodes
  "Adds all possible nodes matching attributes (format like query) to the graph."
  ([graph attributes]
   (reduce add-node graph (map-cross attributes))))

(defn add-edges
  "Adds all possible nodes matching attributes (format like query) to the graph."
  ([graph attributes]
   (reduce add-edge graph (map-cross attributes))))

(defn get-edges
  "Gets the values of all node keys given from the graph."
  ([graph node-keys]
   (map (partial get-edge graph) (edges graph))))
