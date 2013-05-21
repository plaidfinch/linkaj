(ns map-experiments.directed-graph.core
  (:require [map-experiments.directed-graph.protocol :refer :all]
            [map-experiments.directed-graph.macro    :refer :all]
            [map-experiments.smart-maps              :refer :all]
            [clojure.set                             :refer :all])
  (:import [clojure.lang
            IPersistentMap IPersistentSet IPersistentCollection ILookup IFn IObj IMeta Associative MapEquivalence Seqable]))

(declare edges-touching)

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

(defn- parse-relations
  ([attributes relations-map]
   (let [relations
         (select-keys attributes
                      (concat (keys relations-map)
                              (keys (inverse relations-map))))
         rest-attrs (apply dissoc attributes (keys relations))]
        [relations rest-attrs])))

; The type definition itself!

(deftype DirectedGraph [nodes-set
                        nodes-map
                        edges-map
                        node-id-seq
                        edge-id-seq
                        relations-map
                        constraints-fn
                        metadata]
  
  IDirectedGraph
  
  ; Methods acting on nodes:
  (nodes [this]
         (when (< 0 (count nodes-set)) nodes-set))
  (nodes [this query]
         (if (not (seq query))
             (nodes this)
             (apply intersection
                    (for [[a vs] query]
                         (if (relation-in? this a)
                             (apply (comp set union)
                                    (for [v vs]
                                         (map #(attr-get
                                                 edges-map % (opposite relations-map a))
                                              (keys-with edges-map a v))))
                             (apply union
                                    (for [v vs]
                                         (keys-with nodes-map a v))))))))
  (node? [this o]
         (contains? nodes-set o))
  (get-node [this node-key]
            (if (node? this node-key)
                (if-let [return (get nodes-map node-key)]
                        return
                        {})))
  (add-node [this attributes]
            (if (or (key-overlap? attributes relations-map)
                    (key-overlap? attributes (inverse relations-map)))
                (throw (IllegalArgumentException.
                         "Attributes may not be identical to existing relations"))
                (let [node-key (first node-id-seq)]
                     (#(constraints-fn % node-key)
                        (DirectedGraph.
                          (conj nodes-set node-key)
                          (assoc nodes-map node-key attributes)
                          edges-map
                          (rest node-id-seq)
                          edge-id-seq relations-map constraints-fn metadata)))))
  (remove-node [this node-key]
               (let [edges-to-remove (edges-touching this node-key)]
                    (#(constraints-fn % node-key)
                       (DirectedGraph.
                         (disj nodes-set node-key)
                         (dissoc nodes-map node-key)
                         (apply dissoc edges-map edges-to-remove)
                         (if (node? this node-key)
                             (cons node-key node-id-seq)
                             node-id-seq)
                         (concat edges-to-remove edge-id-seq)
                         relations-map constraints-fn metadata))))
  (assoc-node [this node-key attributes]
              (if (cond (or (key-overlap? attributes relations-map)
                            (key-overlap? attributes (inverse relations-map)))
                        (throw (IllegalArgumentException.
                                 "Attributes may not be existing relations"))
                        (not (node? this node-key))
                        (throw (IllegalArgumentException.
                                 "Node must exist before assoc-ing onto it; to create a new node with attributes, use add-node"))
                        :else true)
                  (#(constraints-fn % node-key)
                     (DirectedGraph.
                       nodes-set
                       (assoc nodes-map node-key attributes)
                       edges-map node-id-seq edge-id-seq relations-map constraints-fn metadata))))
  (dissoc-node [this node-key attribute-keys]
               (let [new-nodes-map (reduce #(attr-dissoc %1 node-key %2)
                                           nodes-map attribute-keys)]
                    (#(constraints-fn % node-key)
                       (DirectedGraph.
                         nodes-set
                         new-nodes-map
                         edges-map node-id-seq edge-id-seq relations-map constraints-fn metadata))))
  
  ; Methods acting on edges:
  (edges [this]
         (when (< 0 (count edges-map))
               (apply hash-set (keys edges-map))))
  (edges [this query]
         (if (not (seq query))
             (edges this)
             (apply intersection
                    (for [[a vs] query]
                         (apply union
                                (for [v vs]
                                     (keys-with edges-map a v)))))))
  (edge? [this o]
         (contains? edges-map o))
  (get-edge [this edge-key]
            (get edges-map edge-key))
  (add-edge [this attributes]
            ; Validating that edge has exactly two relations, and they point to existing nodes in the graph
            (if (let [[relations rest-attrs] (parse-relations attributes relations-map)]
                     (if (not= 2 (count relations))
                         (throw (IllegalArgumentException.
                                  "An edge must have relations to exactly two nodes"))
                         (let [[r1 r2] (keys relations)]
                              (cond (not (= r1 (opposite relations-map r2)))
                                    (throw (IllegalArgumentException.
                                             "Relations for an edge must be opposites"))
                                    (not (and (node? this (relations r1))
                                              (node? this (relations r2))))
                                    (throw (IllegalArgumentException.
                                             "Edges must connect existing nodes"))
                                    :else true))))
                (let [edge-key (first edge-id-seq)]
                     (#(constraints-fn % edge-key)
                        (DirectedGraph.
                          nodes-set nodes-map
                          (assoc edges-map edge-key attributes)
                          node-id-seq
                          (rest edge-id-seq)
                          relations-map constraints-fn metadata)))))
  (remove-edge [this edge-key]
               (#(constraints-fn % edge-key)
                  (DirectedGraph.
                    nodes-set nodes-map
                    (dissoc edges-map edge-key)
                    node-id-seq
                    (if (edge? this edge-key)
                        (cons edge-key edge-id-seq)
                        edge-id-seq)
                    relations-map constraints-fn metadata)))
  (assoc-edge [this edge-key attributes]
              ; Massive validation step to check that the new attributes don't violate the conditions of being a properly formed edge...
              (if (let [[relations rest-attrs]
                        (parse-relations attributes relations-map)]
                       (case (count relations)
                             0 (assoc edges-map edge-key attributes)
                             1 (let [r1 (key (first relations))]
                                    (cond (not (attr-get edges-map
                                                         edge-key
                                                         (opposite relations-map r1)))
                                          (throw (IllegalArgumentException.
                                                   "Edge relations must be opposites"))
                                          (not (node? this (relations r1)))
                                          (throw (IllegalArgumentException.
                                                   "Edges must connect existing nodes"))
                                          :else true))
                             2 (let [[r1 r2] (keys relations)]
                                    (cond (not (= r1 (opposite relations-map r2)))
                                          (throw (IllegalArgumentException.
                                                   "Edge relations must be opposites"))
                                          (not (attr-get edges-map
                                                         edge-key
                                                         (opposite relations-map r1)))
                                          (throw (IllegalArgumentException.
                                                   "The type of relation for an edge may not be altered."))
                                          (not (and (node? this (relations r1))
                                                    (node? this (relations r2))))
                                          (throw (IllegalArgumentException.
                                                   "Edges must connect existing nodes"))
                                          :else true))
                             (throw (IllegalArgumentException.
                                      "Edges must be related to exactly 2 nodes."))))
                  (if (not (edge? this edge-key))
                      this
                      (#(constraints-fn % edge-key)
                         (DirectedGraph.
                           nodes-set nodes-map
                           (assoc edges-map edge-key attributes)
                           node-id-seq edge-id-seq relations-map constraints-fn metadata)))))
  (dissoc-edge [this edge-key attribute-keys]
               ; Validate that there are no relations being dissoced
               (let [[relations rest-attrs]
                     (parse-relations
                       (into {} (map vector attribute-keys (repeat nil)))
                       relations-map)]
                    (if (not= 0 (count relations))
                        (throw (IllegalArgumentException.
                                 "An edge cannot be disconnected from a node without being connected to another node"))
                        (#(constraints-fn % edge-key)
                           (DirectedGraph.
                             nodes-set nodes-map
                             (reduce #(attr-dissoc %1 edge-key %2)
                                     edges-map attribute-keys)
                             node-id-seq edge-id-seq relations-map constraints-fn metadata)))))
  
  Relational
  (relations [this] relations-map)
  (related-in? [this r1 r2]
               (and (relation-in? this r1)
                    (relation-in? this r2)
                    (= r1 (opposite relations-map r2))))
  (relation-in? [this r]
                (or (contains? relations-map r)
                    (contains? (inverse relations-map) r)))
  (add-relation [this r1 r2]
                (DirectedGraph.
                  nodes-set nodes-map edges-map node-id-seq edge-id-seq
                  (assoc relations-map r1 r2)
                  constraints-fn metadata))
  (remove-relation [this r1 r2]
                   (if (and (related-in? this r1 r2)
                            (nil? (keys-with-attr edges-map r1))
                            (nil? (keys-with-attr edges-map r2)))
                       (DirectedGraph.
                         nodes-set nodes-map edges-map node-id-seq edge-id-seq
                         (dissoc (rdissoc relations-map r1) r1)
                         constraints-fn metadata)
                       (throw (IllegalArgumentException.
                                "Relation could not be removed from graph for one of the following reasons: a) the two relations given are not each others' opposites; b) there are existing edges along this relation"))))
  
  Constrained
  (add-constraint [this f]
                  (DirectedGraph.
                    nodes-set nodes-map edges-map node-id-seq edge-id-seq relations-map
                    (fn [graph k] (f (constraints-fn graph k) k))
                    metadata))
  (reset-constraints [this]
                     (DirectedGraph.
                       nodes-set nodes-map edges-map node-id-seq edge-id-seq relations-map
                       (fn [graph k] graph)
                       metadata))
  (verify-constraints [this] "NOT YET IMPLEMENTED")
  
  ILookup
  (valAt [this k]
         (or (get-edge this k) (get-node this k)))
  (valAt [this k not-found]
         (if (contains? this k)
             (get this k)
             not-found))
  
  Associative
  (containsKey [this k]
               (or (edge? this k)
                   (node? this k)))
  (entryAt [this k]
           (when (contains? this k)
                 (clojure.lang.MapEntry. k (get this k))))
  
  IMeta
  (meta [this] metadata)
  
  IObj
  (withMeta [this new-meta]
            (DirectedGraph.
              nodes-set nodes-map edges-map node-id-seq edge-id-seq relations-map constraints-fn
              new-meta)))

(defn digraph
  ([] (DirectedGraph.
        (hash-set)
        (attr-map)
        (attr-map)
        (iterate (comp inc inc) 0) ; all node keys are even numbers
        (iterate (comp inc inc) 1) ; all edge keys are odd numbers
        (bijection)
        (fn [graph k] graph) ; the initial constraint does nothing
        (hash-map)))
  ([& {:keys [relations constraints]}]
   (reduce add-constraint
           (reduce (partial apply add-relation) (digraph) relations)
           constraints)))

; Additional methods for semantic ease...

; Singular selectors for nodes and edges:

(def node
  "For selecting a single node when you know the query is unique."
  (specific nodes))

(def edge
  "For selecting a single edge when you know the query is unique."
  (specific edges))

; Plural operators for nodes:

(defn get-nodes
  "Gets the values of all node keys given from the graph."
  ([graph node-keys]
   (map (partial get-node graph) (nodes graph))))

(defn add-nodes
  "Adds all possible nodes matching attributes (format like query) to the graph."
  ([graph attributes]
   (reduce add-node graph (map-cross attributes))))

(defn remove-nodes
  "Removes all nodes in node-keys from the graph."
  ([graph node-keys attributes]
   (reduce remove-node graph node-keys)))

(defn assoc-nodes
  "Associates all nodes in node-keys with the attributes."
  ([graph node-keys attributes]
   (reduce #(assoc-node %1 %2 attributes) graph node-keys)))

(defn dissoc-nodes
  "Dissociates all nodes in node-keys from the attribute-keys."
  ([graph node-keys attribute-keys]
   (reduce #(dissoc-node %1 %2 attribute-keys) graph node-keys)))

; Plural operators for edges:

(defn get-edges
  "Gets the values of all node keys given from the graph."
  ([graph node-keys]
   (map (partial get-edge graph) (edges graph))))

(defn add-edges
  "Adds all possible nodes matching attributes (format like query) to the graph."
  ([graph attributes]
   (reduce add-edge graph (map-cross attributes))))

(defn remove-edges
  "Removes all edges in edge-keys from the graph."
  ([graph edge-keys attributes]
   (reduce remove-edge graph edge-keys)))

(defn assoc-edges
  "Associates all edges in edge-keys with the attributes."
  ([graph edge-keys attributes]
   (reduce #(assoc-edge %1 %2 attributes) graph edge-keys)))

(defn dissoc-edges
  "Dissociates all edges in edge-keys from the attribute-keys."
  ([graph edge-keys attribute-keys]
   (reduce #(dissoc-edge %1 %2 attribute-keys) graph edge-keys)))

; Other useful operators:

(defn edges-touching
  "Finds all edges which are connected by any relation to a particular node."
  ([graph node-key]
   (mapcat #(g-> graph (edges {% [node-key]}))
           (mapcat identity (relations graph)))))

(defn get-all
  "Gets every node or edge (usually all one or the other) in a sequence of keys."
  ([graph ks]
   (map (partial get graph) ks)))

(defn relate
  "Creates an edge between n1 and n2 related to n1 by rel and to n2 by its opposite. More succinct in some cases than add-edge. Gives the edge attributes, if any."
  ([graph rel n1 n2]
   (relate graph rel n1 n2 {}))
  ([graph rel n1 n2 attributes]
   (g-> graph
        (add-edge (g-| (assoc attributes
                              rel n1
                              (opposite (relations graph) rel) n2))))))
