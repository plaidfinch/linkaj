(ns map-experiments.directed-graph.core
  (:require [map-experiments.directed-graph.protocol :refer :all]
            [map-experiments.directed-graph.macro    :refer :all]
            [map-experiments.smart-maps              :refer :all]
            [clojure.set                             :refer :all])
  (:import [clojure.lang
            IPersistentMap IPersistentSet IPersistentCollection ILookup IFn IObj IMeta Associative MapEquivalence Seqable]))

(declare edges-touching starting-node-seq starting-edge-seq)

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

; GraphNodes are ephemeral maps which contain a hidden id. They are emitted from node queries and their keys/values are looked up lazily, which means that one can efficiently map over a set of GraphNodes without the program having to look up every value in each node.

(deftype GraphNode [metadata graph id nodes-map]
  IComponent
  (id [this] id)
  IPersistentMap
  (assoc [this k v] (with-meta (assoc (get nodes-map id) k v) metadata))
  (without [this k] (with-meta (dissoc (get nodes-map id) k) metadata))
  IPersistentCollection
  (cons [this x] (with-meta (conj (get nodes-map id) x) metadata))
  (equiv [this o]
         (and (isa? (class o) GraphNode)
              (= id (.id ^GraphNode o))
              (= nodes-map (.nodes-map ^GraphNode o))))
  (empty [this] (with-meta {} metadata))
  IObj (withMeta [this new-meta] (GraphNode. new-meta graph id nodes-map))
  Associative
  (containsKey [this k] (if (attr-get nodes-map id k) true false))
  (entryAt [this k]
           (when (contains? this k)
                 (clojure.lang.MapEntry. k (attr-get nodes-map id k))))
  ILookup
  (valAt [this k] (attr-get nodes-map id k))
  (valAt [this k not-found] (attr-get nodes-map id k not-found))
  Seqable (seq [this] (seq (get nodes-map id)))
  IFn (invoke [this k] (get this k))
  IMeta (meta [this] metadata)
  Object (toString [this] (str (get nodes-map id)))
  MapEquivalence)

(defn node? [x]
  (instance? GraphNode x))

(defn- graph-node [graph nodes-map id]
  (GraphNode. nil graph id nodes-map))

(defn- make-edge-map [graph id nodes-map edges-map relations-map]
  (let [edge-map (get edges-map id)
        rels (keys (select-keys edge-map (mapcat identity relations-map)))]
       (reduce conj edge-map
               (map #(vector % (graph-node graph nodes-map (get edge-map %)))
                    rels))))

(deftype GraphEdge [metadata graph id nodes-map edges-map relations-map]
  IComponent
  (id [this] id)
  IPersistentMap
  (assoc [this k v]
         (with-meta
           (assoc (make-edge-map graph id nodes-map edges-map relations-map) k v)
           metadata))
  (without [this k]
           (with-meta
             (dissoc (make-edge-map graph id nodes-map edges-map relations-map) k)
             metadata))
  IPersistentCollection
  (cons [this x]
        (with-meta
          (conj (make-edge-map graph id nodes-map edges-map relations-map) x)
          metadata))
  (equiv [this o]
         (and (isa? (class o) GraphEdge)
              (= id (.id ^GraphEdge o))
              (= edges-map (.edges-map ^GraphEdge o))))
  (empty [this] (with-meta {} metadata))
  (count [this] (count (make-edge-map graph id nodes-map edges-map relations-map)))
  IObj (withMeta [this new-meta] (GraphEdge. new-meta graph id nodes-map edges-map relations-map))
  Associative
  (containsKey [this k]
               (if (attr-get edges-map id k) true false))
  (entryAt [this k]
           (when (contains? this k)
                 (clojure.lang.MapEntry. k (get this k))))
  ILookup
  (valAt [this k] (get this k nil))
  (valAt [this k not-found]
         (if (contains? this k)
             (let [v (attr-get edges-map id k)]
                  (if (relation-in? graph k)
                      (if (node-in? graph (graph-node graph nodes-map v))
                          (graph-node graph nodes-map v)
                          (throw (IllegalStateException.
                                   "Illegal internal state in graph edge: stored node does not match stored graph!")))
                      v))
             not-found))
  Seqable
  (seq [this]
       (seq (make-edge-map graph id nodes-map edges-map relations-map)))
  Object
  (toString [this]
            (str (make-edge-map graph id nodes-map edges-map relations-map)))
  IFn (invoke [this k] (get this k))
  IMeta (meta [this] metadata)
  MapEquivalence)

(defn edge? [x]
  (instance? GraphEdge x))

(defn- graph-edge [graph nodes-map edges-map relations-map id]
  (GraphEdge. nil graph id nodes-map edges-map relations-map))

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
         (map (partial graph-node this nodes-map)
              (when (< 0 (count nodes-set)) nodes-set)))
  (nodes [this query]
         (map (partial graph-node this nodes-map)
              (if (not (seq query))
                  (nodes this)
                  (apply intersection
                         (for [[a vs] query]
                              (if (relation-in? this a)
                                  (apply (comp set union)
                                         (for [v vs]
                                              (map #(attr-get
                                                      edges-map %
                                                      (opposite relations-map a))
                                                   (keys-with edges-map a (id v)))))
                                  (apply (comp set union)
                                         (for [v vs]
                                              (keys-with nodes-map a v)))))))))
  (node-in? [this o]
            (and (node? o)
                 (= this (.graph ^GraphNode o))
                 (contains? nodes-set (id o))))
  (add-node [this attributes]
            (if (or (key-overlap? attributes relations-map)
                    (key-overlap? attributes (inverse relations-map)))
                (throw (IllegalArgumentException.
                         "Attributes may not be identical to existing relations"))
                (let [node-key (first node-id-seq)
                      new-nodes-map (assoc nodes-map node-key attributes)]
                     (#(constraints-fn
                         % (graph-node % node-key new-nodes-map))
                        (DirectedGraph.
                          (conj nodes-set node-key)
                          new-nodes-map
                          edges-map
                          (rest node-id-seq)
                          edge-id-seq relations-map constraints-fn metadata)))))
  (remove-node [this n]
               (if (not (node-in? this n))
                   (throw (IllegalArgumentException.
                            "Cannot remove node whose origin is another graph."))
                   (let [node-key (id n)
                         edges-to-remove (edges-touching this n)
                         new-nodes-map (dissoc nodes-map node-key)]
                        (#(constraints-fn
                            % (graph-node % node-key new-nodes-map))
                           (DirectedGraph.
                             (disj nodes-set node-key)
                             new-nodes-map
                             (apply dissoc edges-map edges-to-remove)
                             (if (node-in? this n)
                                 (cons node-key node-id-seq)
                                 node-id-seq)
                             (concat edges-to-remove edge-id-seq)
                             relations-map constraints-fn metadata)))))
  (assoc-node [this n attributes]
              (if (not (node-in? this n))
                  (throw (IllegalArgumentException.
                           "Cannot assoc to node whose origin is in another graph."))
                  (let [node-key (id n)
                        new-nodes-map (assoc nodes-map node-key attributes)]
                       (if (cond (or (key-overlap? attributes relations-map)
                                     (key-overlap? attributes (inverse relations-map)))
                                 (throw (IllegalArgumentException.
                                          "Attributes may not be existing relations"))
                                 (not (node-in? this n))
                                 (throw (IllegalArgumentException.
                                          "Node must exist before assoc-ing onto it; to create a new node with attributes, use add-node"))
                                 :else true)
                           (#(constraints-fn
                               % (graph-node % node-key new-nodes-map))
                              (DirectedGraph.
                                nodes-set
                                new-nodes-map
                                edges-map node-id-seq edge-id-seq relations-map constraints-fn metadata))))))
  (dissoc-node [this n attribute-keys]
               (if (not (node-in? this n))
                   (throw (IllegalArgumentException.
                            "Cannot dissoc from node with origin is in another graph."))
                   (let [node-key (id n)
                         new-nodes-map (reduce #(attr-dissoc %1 node-key %2)
                                               nodes-map attribute-keys)]
                        (#(constraints-fn
                            % (graph-node % node-key new-nodes-map))
                           (DirectedGraph.
                             nodes-set
                             new-nodes-map
                             edges-map node-id-seq edge-id-seq relations-map constraints-fn metadata)))))

  ; Methods acting on edges:
  (edges [this]
         (map (partial graph-edge this nodes-map edges-map relations-map)
              (when (< 0 (count edges-map))
                    (apply hash-set (keys edges-map)))))
  (edges [this query]
         (map (partial graph-edge this nodes-map edges-map relations-map)
              (if (not (seq query))
                  (edges this)
                  (apply intersection
                         (for [[a vs] query]
                              (if (relation-in? this a)
                                  (apply (comp set union)
                                         (for [v vs]
                                              (keys-with edges-map a (id v))))
                                  (apply (comp set union)
                                         (for [v vs]
                                              (keys-with edges-map a v)))))))))
  (edge-in? [this o]
            (and (edge? o)
                 (= this (.graph ^GraphEdge o))
                 (contains? edges-map (id o))))
  (add-edge [this attributes]
            ; Validating that edge has exactly two relations, and they point to existing nodes in the graph
            (let [[relations rest-attrs] (parse-relations attributes relations-map)]
                 (if (cond (< (count relations) 2)
                           (throw (IllegalArgumentException.
                                    "An edge cannot be created without a relation to exactly 2 existing nodes"))
                           (= (count relations) 2)
                           (let [[r1 r2] (keys relations)]
                                (cond (not (= r1 (opposite relations-map r2)))
                                      (throw (IllegalArgumentException.
                                               "Relations for edges must be opposites"))
                                      (not (and (node-in? this (relations r1))
                                                (node-in? this (relations r2))))
                                      (throw (IllegalArgumentException.
                                               "Edges must connect existing nodes"))
                                      :else true))
                           :else (throw (IllegalArgumentException.
                                          "An edge cannot have relations to greater than two nodes")))
                     (let [edge-key
                           (first edge-id-seq)
                           new-edges-map
                           (assoc edges-map edge-key
                                  (merge rest-attrs
                                         (zipmap (keys relations)
                                                 (map id (vals relations)))))]
                          (#(constraints-fn
                              % (graph-edge % nodes-map new-edges-map relations-map edge-key))
                             (DirectedGraph.
                               nodes-set nodes-map
                               new-edges-map
                               node-id-seq
                               (rest edge-id-seq)
                               relations-map constraints-fn metadata))))))
            (remove-edge [this e]
               (if (not (edge-in? this e))
                   (throw (IllegalArgumentException.
                            "Cannot remove edge with origin in another graph."))
                   (let [edge-key (id e)
                         new-edges-map (dissoc edges-map edge-key)]
                        (#(constraints-fn
                            % (graph-edge % nodes-map new-edges-map relations-map edge-key))
                           (DirectedGraph.
                             nodes-set nodes-map
                             new-edges-map
                             node-id-seq
                             (if (edge-in? this edge-key)
                                 (cons edge-key edge-id-seq)
                                 edge-id-seq)
                             relations-map constraints-fn metadata)))))
  (assoc-edge [this e attributes]
              ; Massive validation step to check that the new attributes don't violate the conditions of being a properly formed edge...
              (let [edge-key (id e)
                    new-edges-map (assoc edges-map edge-key attributes)]
                   (cond (not (edge-in? this e))
                         (throw (IllegalArgumentException.
                                  "Cannot remove edge with origin in another graph."))
                         (let [[relations rest-attrs]
                               (parse-relations attributes relations-map)
                               [r1 r2] (keys relations)]
                              (case (count relations)
                                    0 true
                                    1 (cond (not (attr-get edges-map edge-key
                                                           (opposite relations-map r1)))
                                            (throw (IllegalArgumentException.
                                                     "The type of relation for an edge may not be altered."))
                                            (not (node-in? this (relations r1)))
                                            (throw (IllegalArgumentException.
                                                     "Edges must be connected to existing nodes"))
                                            :else true)
                                    2 (cond (or (not (= r1 (opposite relations-map r2)))
                                                (not (attr-get edges-map edge-key
                                                               (opposite relations-map r1))))
                                            (throw (IllegalArgumentException.
                                                     "The type of relation for an edge may not be altered."))
                                            (not (and (node-in? this (relations r1))
                                                      (node-in? this (relations r2))))
                                            (throw (IllegalArgumentException.
                                                     "Edges must be connected to existing nodes"))
                                            :else true)
                                    (throw (IllegalArgumentException.
                                             "Edges must relate to exactly 2 nodes"))))
                         (#(constraints-fn
                             % (graph-edge % nodes-map new-edges-map relations-map edge-key))
                            (DirectedGraph.
                              nodes-set nodes-map
                              new-edges-map
                              node-id-seq edge-id-seq relations-map constraints-fn metadata)))))
  (dissoc-edge [this e attribute-keys]
               ; Validate that there are no relations being dissoced
               (if (not (edge-in? this e))
                   (throw (IllegalArgumentException.
                            "Cannot remove edge with origin in another graph."))
                   (let [edge-key (id e)
                         new-edges-map (reduce #(attr-dissoc %1 edge-key %2)
                                               edges-map attribute-keys)
                         [relations rest-attrs]
                         (parse-relations
                           (into {} (map vector attribute-keys (repeat nil)))
                           relations-map)]
                        (if (not= 0 (count relations))
                            (throw (IllegalArgumentException.
                                     "An edge cannot be disconnected from a node without being connected to another node"))
                            (#(constraints-fn
                                % (graph-edge % nodes-map new-edges-map relations-map edge-key))
                               (DirectedGraph.
                                 nodes-set nodes-map
                                 new-edges-map
                                 node-id-seq edge-id-seq relations-map constraints-fn metadata))))))
  
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
  (verify-constraints [this]
                      (reduce constraints-fn
                              this
                              (concat (nodes this) (edges this))))
  
  IPersistentCollection
  (equiv [this o] 
         (or (and (isa? (class o) DirectedGraph)
                  (= nodes-set      (.nodes-set      ^DirectedGraph o))
                  (= nodes-map      (.nodes-map      ^DirectedGraph o))
                  (= edges-map      (.edges-map      ^DirectedGraph o))
                  (= node-id-seq    (.node-id-seq    ^DirectedGraph o))
                  (= edge-id-seq    (.edge-id-seq    ^DirectedGraph o))
                  (= relations-map  (.relations-map  ^DirectedGraph o)))))
  (empty [this]
         (DirectedGraph.
           (empty nodes-set)
           (empty nodes-map)
           (empty edges-map)
           (starting-node-seq)
           (starting-edge-seq)
           relations-map
           constraints-fn
           metadata))
  
  IPersistentMap
  (assoc [this k attributes]
         (cond (node-in? this k) (assoc-node this k attributes)
               (edge-in? this k) (assoc-edge this k attributes)
               :else this))
  
  Seqable
  (seq [this]
       (seq {:relations relations-map
             :nodes (-#> this nodes)
             :edges (-#> this edges)}))
  
  Associative
  (containsKey [this k]
               (or (edge-in? this k)
                   (node-in? this k)))
  (entryAt [this k]
           (when (contains? this k)
                 (clojure.lang.MapEntry. k (get this k))))
  
  Object
  (toString [this]
            (str (into {} (seq this))))
  
  IMeta
  (meta [this] metadata)
  
  IObj
  (withMeta [this new-meta]
            (DirectedGraph.
              nodes-set nodes-map edges-map node-id-seq edge-id-seq relations-map constraints-fn
              new-meta)))

; all node keys are even numbers
(defn- starting-node-seq []
  (iterate (comp inc inc) 0))
; all edge keys are odd numbers
(defn- starting-edge-seq []
  (iterate (comp inc inc) 1))

(defn digraph
  ([] (DirectedGraph.
        (hash-set)
        (attr-map)
        (attr-map)
        (starting-node-seq)
        (starting-edge-seq)
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

(defn add-nodes
  "Adds all possible nodes matching attributes (format like query) to the graph."
  ([graph attributes]
   (reduce add-node graph (map-cross attributes))))

(defn remove-nodes
  "Removes all nodes in ns from the graph."
  ([graph ns attributes]
   (reduce remove-node graph ns)))

(defn assoc-nodes
  "Associates all nodes in ns with the attributes."
  ([graph ns attributes]
   (reduce #(assoc-node %1 %2 attributes) graph ns)))

(defn dissoc-nodes
  "Dissociates all nodes in ns from the attribute-keys."
  ([graph ns attribute-keys]
   (reduce #(dissoc-node %1 %2 attribute-keys) graph ns)))

; Plural operators for edges:

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
  ([graph n]
   (mapcat #(-#> graph (edges {% [n]}))
           (mapcat identity (relations graph)))))

(defn assoc-all
  "Associates every item (edge or node) with the attributes."
  ([graph ks attributes]
   (reduce #(assoc %1 %2 attributes) graph ks)))

(defn relate
  "Creates an edge between n1 and n2 related to n1 by rel and to n2 by its opposite. More succinct in some cases than add-edge. Gives the edge attributes, if any."
  ([graph rel n1 n2]
   (relate graph rel n1 n2 {}))
  ([graph rel n1 n2 attributes]
   (-#> graph
        (add-edge (-#| (assoc attributes
                              rel n1
                              (opposite (relations graph) rel) n2))))))
