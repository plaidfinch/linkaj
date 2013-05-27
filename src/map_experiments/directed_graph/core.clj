(ns map-experiments.directed-graph.core
  (:require [map-experiments.directed-graph.protocol :refer :all]
            [map-experiments.directed-graph.macro    :refer :all]
            [map-experiments.smart-maps              :refer :all]
            [clojure.set                             :refer :all])
  (:import [clojure.lang
            IPersistentMap IPersistentSet IPersistentCollection ILookup IFn IObj IMeta Associative MapEquivalence Seqable]))

(declare
  edges-touching
  starting-node-seq starting-edge-seq
  remove-edges remove-nodes
  graph-node graph-edge
  node? edge?)

; Some useful functions which might be of use to people doing other things:

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
  "Takes a map where keys are sequences are returns a sequence of maps where keys are every possible pick of a key for each value (cartesian product analogue for maps of sequences). If any key is not a sequence, it is treated as if it is a sequence of one item."
  ([m]
   (when (seq m)
         (if-let [[k x] (first m)]
                 (let [vs (if (sequential? x) x [x])]
                      (if (seq (dissoc m k))
                          (for [v vs
                                r (map-cross (dissoc m k))]
                               ((fnil conj {}) r [k v]))
                          (for [v vs]
                               (hash-map k v))))))))

; Parse-relations takes a list of attributes and a relations-map and returns a 2-tuple of maps: the relations contained in the map, and all other attributes in the map. It's private as there's little to no use for it outside the type definition.

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
  (nodes* [this]
          (map (partial graph-node this)
               (when (< 0 (count nodes-set)) nodes-set)))
  (nodes* [this query]
          (if (not (seq query))
              (nodes* this)
              (seq 
                (map (partial graph-node this)
                     (apply intersection
                            (for [[a x] query]
                                 (let [vs (if (sequential? x) x [x])]
                                      (apply (comp set union)
                                             (if (relation-in? this a)
                                                 (for [v vs]
                                                      (cond (node? v)
                                                            (map #(attr-get edges-map % (opposite relations-map a))
                                                                 (keys-with edges-map a (id v)))
                                                            (edge? v)
                                                            [(attr-get edges-map (id v) (opposite relations-map a))]
                                                            :else
                                                            (throw (IllegalArgumentException.
                                                                     "Nodes can only be related to nodes, and by extension, to edges."))))
                                                 (for [v vs]
                                                      (keys-with nodes-map a v)))))))))))
  (node-in? [this o]
            (and (node? o)
                 ; The below rigorous check breaks any method which wants to reduce a graph across a set of nodes. If you're really worried about making sure nodes and edges don't cross-influence different graphs, I have two pieces of advice:
                 ; a) You can use the API in a sensible way! It should be rather difficult to accidentally cross nodes and edges between two separate graphs, unless you're trying to do so.
                 ; b) You can uncomment this line (and the similar one in edge-in?) to enable these integrity checks, at the cost of losing the use of every plural mutator method like those at the bottom of the file.
                 ;(= this (.graph ^GraphNode o))
                 (contains? nodes-set (id o))))
  (add-node* [this attributes]
             (cond (or (key-overlap? attributes relations-map)
                       (key-overlap? attributes (inverse relations-map)))
                   (throw (IllegalArgumentException.
                            "Attributes may not be identical to existing relations"))
                   (not (seq node-id-seq))
                   (throw (IllegalStateException.
                            "Empty internal node id sequence; check custom specifications for this parameter to ensure that sequence specified is infinite"))
                   (contains? nodes-map (first node-id-seq))
                   (throw (IllegalStateException.
                            "Encountered duplicate in internal node id sequence; check custom specifications for this parameter to ensure that sequence specified is non-repeating"))
                   :else
                   (let [node-key (first node-id-seq)
                         new-nodes-map (assoc nodes-map node-key attributes)]
                        (#(constraints-fn
                            :add this % (graph-node % node-key))
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
                        (if (seq edges-to-remove)
                            (let [new-graph (remove-edges this edges-to-remove)]
                                 (remove-node new-graph
                                              (graph-node new-graph (id n))))
                            (#(constraints-fn
                                :remove this % (graph-node % node-key))
                               (DirectedGraph.
                                 (disj nodes-set node-key)
                                 new-nodes-map
                                 edges-map
                                 (if (node-in? this n)
                                     (cons node-key node-id-seq)
                                     node-id-seq)
                                 edge-id-seq relations-map constraints-fn metadata))))))
  (assoc-node* [this n attributes]
               (if (not (node-in? this n))
                   (throw (IllegalArgumentException.
                            "Cannot assoc to node whose origin is in another graph."))
                   (let [node-key (id n)
                         new-nodes-map (assoc nodes-map node-key attributes)]
                        (cond (or (key-overlap? attributes relations-map)
                                  (key-overlap? attributes (inverse relations-map)))
                              (throw (IllegalArgumentException.
                                       "Attributes may not be existing relations"))
                              (not (node-in? this n))
                              (throw (IllegalArgumentException.
                                       "Node must exist before assoc-ing onto it; use add-node to create a new node with attributes"))
                              :else
                              (#(constraints-fn
                                  :assoc this % (graph-node % node-key))
                                 (DirectedGraph.
                                   nodes-set
                                   new-nodes-map
                                   edges-map node-id-seq edge-id-seq relations-map constraints-fn metadata))))))
  (dissoc-node* [this n attribute-keys]
                (if (not (node-in? this n))
                    (throw (IllegalArgumentException.
                             "Cannot dissoc from node with origin in another graph."))
                    (let [node-key (id n)
                          new-nodes-map (reduce #(attr-dissoc %1 node-key %2)
                                                nodes-map attribute-keys)]
                         (#(constraints-fn
                             :dissoc this % (graph-node % node-key))
                            (DirectedGraph.
                              nodes-set
                              new-nodes-map
                              edges-map node-id-seq edge-id-seq relations-map constraints-fn metadata)))))
  
  ; Methods acting on edges:
  (edges* [this]
          (map (partial graph-edge this)
               (when (< 0 (count edges-map))
                     (apply hash-set (keys edges-map)))))
  (edges* [this query]
          (if (not (seq query))
              (edges* this)
              (seq
                (map (partial graph-edge this)
                     (apply intersection
                            (for [[a x] query]
                                 (let [vs (if (sequential? x) x [x])]
                                      (apply (comp set union)
                                             (if (relation-in? this a)
                                                 (for [v vs]
                                                      (cond (node? v)
                                                            (keys-with edges-map a (id v))
                                                            (edge? v)
                                                            (keys-with
                                                              edges-map a
                                                              (attr-get edges-map (id v) (opposite relations-map a)))
                                                            :else
                                                            (throw (IllegalArgumentException.
                                                                     "Edges can only be related to nodes, and by extension, to edges."))))
                                                 (for [v vs]
                                                      (keys-with edges-map a v)))))))))))
  (edge-in? [this o]
            (and (edge? o)
                 ; The below rigorous check breaks any method which wants to reduce a graph across a set of edges. If you're really worried about making sure nodes and edges don't cross-influence different graphs, I have two pieces of advice:
                 ; a) You can use the API in a sensible way! It should be rather difficult to accidentally cross nodes and edges between two separate graphs, unless you're trying to do so.
                 ; b) You can uncomment this line (and the similar one in node-in?) to enable these integrity checks, at the cost of losing the use of every plural mutator method like those at the bottom of the file.
                 ;(= this (.graph ^GraphEdge o))
                 (contains? edges-map (id o))))
  (add-edge* [this attributes]
             ; Validating that edge has exactly two relations, and they point to existing nodes in the graph
             (let [[relations rest-attrs] (parse-relations attributes relations-map)]
                  (if (cond (not (seq edge-id-seq))
                            (throw (IllegalStateException.
                                     "Empty internal edge id sequence; check custom specifications for this parameter to ensure that sequence specified is infinite"))
                            (contains? edges-map (first edge-id-seq))
                            (throw (IllegalStateException.
                                     "Encountered duplicate in internal edge id sequence; check custom specifications for this parameter to ensure that sequence specified is non-repeating"))
                            (< (count relations) 2)
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
                               :add this % (graph-edge % edge-key))
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
                            :remove this % (graph-edge % edge-key))
                           (DirectedGraph.
                             nodes-set nodes-map
                             new-edges-map
                             node-id-seq
                             (if (edge-in? this edge-key)
                                 (cons edge-key edge-id-seq)
                                 edge-id-seq)
                             relations-map constraints-fn metadata)))))
  (assoc-edge* [this e attributes]
               ; Massive validation step to check that the new attributes don't violate the conditions of being a properly formed edge...
               (let [edge-key (id e)
                     new-edges-map (assoc edges-map edge-key attributes)]
                    (cond (not (edge-in? this e))
                          (throw (IllegalArgumentException.
                                   "Cannot assoc on edge with origin in another graph."))
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
                              :assoc this % (graph-edge % edge-key))
                             (DirectedGraph.
                               nodes-set nodes-map
                               new-edges-map
                               node-id-seq edge-id-seq relations-map constraints-fn metadata)))))
  (dissoc-edge* [this e attribute-keys]
                ; Validate that there are no relations being dissoced
                (if (not (edge-in? this e))
                    (throw (IllegalArgumentException.
                             "Cannot dissoc from edge with origin in another graph."))
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
                                 :dissoc this % (graph-edge % edge-key))
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
                    (fn [action old-graph new-graph k]
                      (f old-graph (constraints-fn old-graph new-graph k) k))
                    metadata))
  (reset-constraints [this]
                     (DirectedGraph.
                       nodes-set nodes-map edges-map node-id-seq edge-id-seq relations-map
                       (fn [action old-graph new-graph k] new-graph)
                       metadata))
  (verify-constraints [this]
                      (reduce constraints-fn
                              this
                              (concat (nodes* this) (edges* this))))
  
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
  
  Seqable
  (seq [this]
       (seq {:relations relations-map
             :nodes (-#> this nodes*)
             :edges (-#> this edges*)}))
  
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

; Defining GraphNodes and GraphEdges, which are emitted from the graph in response to queries:

; GraphNodes are ephemeral maps which contain a hidden id. They are emitted from node queries and their keys/values are looked up lazily, which means that one can efficiently map over a set of GraphNodes without the program having to look up every value in each node.

(deftype GraphNode [metadata graph id]
  IComponent
  (id [this] id)
  IPersistentMap
  (assoc [this k v]
         (with-meta (assoc (get (.nodes-map ^DirectedGraph graph) id) k v) metadata))
  (without [this k]
           (with-meta (dissoc (get (.nodes-map ^DirectedGraph graph) id) k) metadata))
  IPersistentCollection
  (cons [this x]
        (with-meta (conj (get (.nodes-map ^DirectedGraph graph) id) x) metadata))
  (equiv [this o]
         (and (isa? (class o) GraphNode)
              (= id (.id ^GraphNode o))
              (= graph (.graph ^DirectedGraph o))))
  (empty [this] (with-meta {} metadata))
  IObj (withMeta [this new-meta]
                 (GraphNode. new-meta graph id))
  Associative
  (containsKey [this k]
               (if (attr-get (.nodes-map ^DirectedGraph graph) id k) true false))
  (entryAt [this k]
           (when (contains? this k)
                 (clojure.lang.MapEntry. k (attr-get (.nodes-map ^DirectedGraph graph) id k))))
  ILookup
  (valAt [this k] (attr-get (.nodes-map ^DirectedGraph graph) id k))
  (valAt [this k not-found] (attr-get (.nodes-map ^DirectedGraph graph) id k not-found))
  Seqable (seq [this] (seq (get (.nodes-map ^DirectedGraph graph) id)))
  IFn (invoke [this k] (get this k))
  IMeta (meta [this] metadata)
  Object (toString [this] (str (get (.nodes-map ^DirectedGraph graph) id)))
  MapEquivalence)

; Forces all the attributes and relations of an edge into a non-lazy map.
(defn- make-edge-map [graph id]
  (let [edge-map (get (.edges-map ^DirectedGraph graph) id)
        rels (keys (select-keys edge-map (mapcat identity (.relations-map ^DirectedGraph graph))))]
       (reduce conj edge-map
               (map #(vector % (graph-node graph (get edge-map %)))
                    rels))))

; A GraphEdge is like a GraphNode. Note that it "contains" GraphNodes as values for its relation keys.

(deftype GraphEdge [metadata graph id]
  IComponent
  (id [this] id)
  IPersistentMap
  (assoc [this k v]
         (with-meta
           (assoc (make-edge-map graph id) k v)
           metadata))
  (without [this k]
           (with-meta
             (dissoc (make-edge-map graph id) k)
             metadata))
  IPersistentCollection
  (cons [this x]
        (with-meta
          (conj (make-edge-map graph id) x)
          metadata))
  (equiv [this o]
         (and (isa? (class o) GraphEdge)
              (= id (.id ^GraphEdge o))
              (= graph (.graph ^GraphEdge o))))
  (empty [this] (with-meta {} metadata))
  (count [this] (count (make-edge-map graph id)))
  IObj (withMeta [this new-meta]
                 (GraphEdge. new-meta graph id))
  Associative
  (containsKey [this k]
               (if (attr-get (.edges-map ^DirectedGraph graph) id k) true false))
  (entryAt [this k]
           (when (contains? this k)
                 (clojure.lang.MapEntry. k (get this k))))
  ILookup
  (valAt [this k] (get this k nil))
  (valAt [this k not-found]
         (if (contains? this k)
             (let [v (attr-get (.edges-map ^DirectedGraph graph) id k)]
                  (if (relation-in? graph k)
                      (graph-node graph v)
                      v))
             not-found))
  Seqable
  (seq [this]
       (seq (make-edge-map graph id)))
  Object
  (toString [this]
            (str (make-edge-map graph id)))
  IFn (invoke [this k] (get this k))
  IMeta (meta [this] metadata)
  MapEquivalence)

; Private constructors for graph nodes and edges. You shouldn't use these; graph nodes and edges will be generated automatically from queries and should not be manually constructed by client code.
(defn- graph-node [graph id]
  (GraphNode. nil graph id))
(defn- graph-edge [graph id]
  (GraphEdge. nil graph id))

; Default configuration for internal node and edge seqs:
(defn- starting-node-seq []
  (repeatedly #(gensym "NODE-")))
(defn- starting-edge-seq []
  (repeatedly #(gensym "EDGE-")))

; Factory function for digraphs:
(defn digraph
  ([& {:keys [relations constraints internal-edge-ids internal-node-ids]}]
   (reduce add-constraint
           (reduce (partial apply add-relation)
                   (DirectedGraph.
                     (hash-set)
                     (attr-map)
                     (attr-map)
                     (or (seq internal-node-ids) (starting-node-seq))
                     (or (seq internal-edge-ids) (starting-edge-seq))
                     (bijection)
                     (fn [action old-graph new-graph k] new-graph)
                     (hash-map))
                   relations)
           constraints)))

; Variadic map-destructing methods for protocol methods which take maps. This means that the syntax for queries can be *much* more succinct. Note that every method here simply destructures its rest arguments as a map and uses the corresponding asterisk method from the protocol.

(defn nodes 
  "Returns all graph nodes matching the query."
  ([graph & query]
   (nodes* graph (apply hash-map query))))
(defn add-node
  "Adds a node with attributes to the graph."
  ([graph & attributes] (add-node* graph (apply hash-map attributes))))
(defn assoc-node
  "Associates node n with attributes."
  ([graph n & attributes] (assoc-node* graph n (apply hash-map attributes))))
(defn dissoc-node
  "Dissociates node n from attribute-keys."
  ([graph n & attribute-ks] (dissoc-node* graph n (apply hash-map attribute-ks))))

(defn edges
  "Returns all graph edges matching the query."
  ([graph & query] (edges* graph (apply hash-map query))))
(defn add-edge
  "Adds an edge with attributes to the graph. Attributes must contain exactly two relations, and they must be each others' opposites."
  ([graph & attributes] (add-edge* graph (apply hash-map attributes))))
(defn assoc-edge
  "Associates edge-key with attributes. This can change relations."
  ([graph n & attributes] (assoc-edge* graph n (apply hash-map attributes))))
(defn dissoc-edge
  "Dissociates edge-key from attribute-keys. Relations cannot be dissociated."
  ([graph n & attribute-ks] (dissoc-edge* graph n (apply hash-map attribute-ks))))

; Additional methods...

; Predicates for being nodes and edges:

(defn node?
  "Tests if its argument is a GraphNode."
  ([x] (instance? GraphNode x)))

(defn edge?
  "Tests if its argument is a GraphEdge."
  ([x] (instance? GraphEdge x)))

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
  ([graph & attributes]
   (reduce add-node* graph (map-cross (apply hash-map attributes)))))

(defn remove-nodes
  "Removes all nodes in xs from the graph."
  ([graph xs]
   (reduce remove-node graph xs)))

(defn assoc-nodes
  "Associates all nodes in xs with the attributes."
  ([graph xs & attributes]
   (reduce #(assoc-node* %1 %2 (apply hash-map attributes)) graph xs)))

(defn dissoc-nodes
  "Dissociates all nodes in xs from the attribute-keys."
  ([graph xs & attribute-keys]
   (reduce #(dissoc-node* %1 %2 (apply hash-map attribute-keys)) graph xs)))

; Plural operators for edges:

(defn add-edges
  "Adds all possible nodes matching attributes (format like query) to the graph."
  ([graph & attributes]
   (reduce add-edge* graph (map-cross (apply hash-map attributes)))))

(defn remove-edges
  "Removes all edges in edge-keys from the graph."
  ([graph es]
   (reduce remove-edge graph es)))

(defn assoc-edges
  "Associates all edges in edge-keys with the attributes."
  ([graph es & attributes]
   (reduce #(assoc-edge* %1 %2 (apply hash-map attributes)) graph es)))

(defn dissoc-edges
  "Dissociates all edges in edge-keys from the attribute-keys."
  ([graph es & attribute-keys]
   (reduce #(dissoc-edge* %1 %2 (apply hash-map attribute-keys)) graph es)))

; Other useful operators:

(defn edges-touching
  "Finds all edges which are connected by any relation to a particular node."
  ([graph n]
   (mapcat #(-#> graph (edges % n))
           (mapcat identity (relations graph)))))

(defn assoc-all
  "Associates every item (edge or node) with the attributes."
  ([graph ks & attributes]
   (reduce #(assoc %1 %2 (apply hash-map attributes)) graph ks)))

(defn relate
  "Creates an edge between n1 and n2 related to n1 by rel and to n2 by its opposite. More succinct in some cases than add-edge. Gives the edge attributes, if any."
  ([graph rel n1 n2 & attributes]
   (let [attrs (apply hash-map attributes)]
        (-#> graph
             (add-edge* (-#| (assoc attrs
                                    rel n1
                                    (opposite (relations graph) rel) n2)))))))

(defn add-path
  "Adds edges between each adjacent node given, along the relation given."
  ([graph rels xs & attributes]
   {:pre (= 2 (count rels))}
   (let [attrs (apply hash-map attributes)]
        (reduce #(add-edge* %1 (merge attrs {(first rels)  (first %2)
                                             (second rels) (second %2)}))
                graph
                (partition 2 1 xs)))))

(defn add-cycle
  "Adds edges between each adjacent node given, along the relation given, and loops back to the first node given."
  ([graph rels xs & attributes]
   {:pre (= 2 (count rels))}
   (let [xs (if (sequential? xs) xs [xs])
         attrs (apply hash-map attributes)]
        (reduce #(add-edge* %1 (merge attrs {(first rels)  (first %2)
                                             (second rels) (second %2)}))
                graph
                (partition 2 1 (concat xs [(first xs)]))))))

(defn nodes-away
  "Finds all nodes which are n edges away from the given set of nodes along the relation given."
  ([graph distance rel xs]
   (let [xs (if (sequential? xs) xs [xs])]
        (cond (< distance 0)
              (nodes-away graph (- distance) (opposite (relations graph) rel) xs)
              (= distance 0) xs
              :else
              (nodes-away graph
                          (dec distance)
                          rel
                          (-#> graph (nodes rel xs)))))))
