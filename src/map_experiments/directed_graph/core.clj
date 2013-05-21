(ns map-experiments.directed-graph.core
  (:require [map-experiments.directed-graph.protocol :refer :all]
            [map-experiments.smart-maps              :refer :all]
            [clojure.set                             :refer :all])
  (:import [clojure.lang
              IPersistentMap IPersistentSet IPersistentCollection ILookup IFn IObj IMeta Associative MapEquivalence Seqable]))

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
         (if (seq query)
             (apply intersection
                    (for [[a vs] query]
                         (apply union (for [v vs] (keys-with nodes-map a v)))))
             (nodes this)))
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
                (DirectedGraph.
                  (conj nodes-set (first node-id-seq))
                  (assoc nodes-map (first node-id-seq) attributes)
                  edges-map
                  (rest node-id-seq)
                  edge-id-seq
                  relations-map
                  constraints-fn
                  metadata)))
  (remove-node [this node-key]
               (assert false "THIS METHOD NEEDS TO BE FIXED: REMOVE CONNECTED EDGES")
               (DirectedGraph.
                 (disj nodes-set node-key)
                 (dissoc nodes-map node-key)
                 edges-map
                 (if (node? this node-key)
                     (cons node-key node-id-seq)
                     node-id-seq)
                 edge-id-seq
                 relations-map
                 constraints-fn
                 metadata))
  (assoc-node [this node-key attributes]
              (if (cond (or (key-overlap? attributes relations-map)
                            (key-overlap? attributes (inverse relations-map)))
                        (throw (IllegalArgumentException.
                                 "Attributes may not be existing relations"))
                        (not (node? this node-key))
                        (throw (IllegalArgumentException.
                                 "Node must exist before assoc-ing onto it; to create a new node with attributes, use add-node"))
                        :else true)
                  (DirectedGraph.
                    nodes-set
                    (assoc nodes-map node-key attributes)
                    edges-map
                    node-id-seq
                    edge-id-seq
                    relations-map
                    constraints-fn
                    metadata)))
  (dissoc-node [this node-key attribute-keys]
               (let [new-nodes-map (reduce #(attr-dissoc %1 node-key %2)
                                           nodes-map attribute-keys)]
                    (DirectedGraph.
                      nodes-set
                      new-nodes-map
                      edges-map
                      node-id-seq
                      edge-id-seq
                      relations-map
                      constraints-fn
                      metadata)))
  
  ; Methods acting on edges:
  (edges [this]
         (when (< 0 (count edges-map))
               (apply hash-set (keys edges-map))))
  (edges [this query]
         (if (seq query)
             (apply intersection
                    (for [[a vs] query]
                         (apply union (for [v vs] (keys-with edges-map a v)))))
             (edges this)))
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
                (DirectedGraph.
                  nodes-set
                  nodes-map
                  (assoc edges-map (first edge-id-seq) attributes)
                  node-id-seq
                  (rest edge-id-seq)
                  relations-map
                  constraints-fn
                  metadata)))
  (remove-edge [this edge-key]
               (DirectedGraph.
                 nodes-set
                 nodes-map
                 (dissoc edges-map edge-key)
                 node-id-seq
                 (if (edge? this edge-key)
                     (cons edge-key edge-id-seq)
                     edge-id-seq)
                 relations-map
                 constraints-fn
                 metadata))
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
                  (DirectedGraph.
                    nodes-set
                    nodes-map
                    (assoc edges-map edge-key attributes)
                    node-id-seq
                    edge-id-seq
                    relations-map
                    constraints-fn
                    metadata)))
  (dissoc-edge [this edge-key attribute-keys]
               ; Validate that there are no relations being dissoced
               (let [[relations rest-attrs]
                     (parse-relations
                       (into {} (map vector attribute-keys (repeat nil)))
                       relations-map)]
                    (if (not= 0 (count relations))
                        (throw (IllegalArgumentException.
                                 "An edge cannot be disconnected from a node without being re-connected to another node"))
                        (DirectedGraph.
                          nodes-set
                          nodes-map
                          (reduce #(attr-dissoc %1 edge-key %2)
                                  edges-map attribute-keys)
                          node-id-seq
                          edge-id-seq
                          relations-map
                          constraints-fn
                          metadata))))
  
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
                  nodes-set
                  nodes-map
                  edges-map
                  node-id-seq
                  edge-id-seq
                  (assoc relations-map r1 r2)
                  constraints-fn
                  metadata))
  (remove-relation [this r1 r2]
                   (if (related-in? this r1 r2)
                       (DirectedGraph.
                         nodes-set
                         nodes-map
                         edges-map
                         node-id-seq
                         edge-id-seq
                         (dissoc (rdissoc relations-map r1) r1)
                         constraints-fn
                         metadata)
                       (throw (IllegalArgumentException.
                                "One or both of the relations specified is not present in the object or is not related to the other relation given."))))
  
  Constrained
  (add-constraint [this f]
                  (DirectedGraph.
                    nodes-set
                    nodes-map
                    edges-map
                    node-id-seq
                    edge-id-seq
                    relations-map
                    (fn [graph k] (f (constraints-fn graph k) k))
                    metadata))
  (reset-constraints [this]
                     (DirectedGraph.
                       nodes-set
                       nodes-map
                       edges-map
                       node-id-seq
                       edge-id-seq
                       relations-map
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
                nodes-set
                nodes-map
                edges-map
                node-id-seq
                edge-id-seq
                relations-map
                constraints-fn
                new-meta)))

(defn digraph []
  (DirectedGraph.
    (hash-set)
    (attr-map)
    (attr-map)
    (iterate (comp inc inc) 0)
    (iterate (comp inc inc) 1)
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

(defn get-edges
  "Gets the values of all node keys given from the graph."
  ([graph node-keys]
   (map (partial get-edge graph) (edges graph))))

(defn add-edges
  "Adds all possible nodes matching attributes (format like query) to the graph."
  ([graph attributes]
   (reduce add-edge graph (map-cross attributes))))

; Special threading macro graph-> to allow automatic context for queries and threading through multiple operations to boot.

; Graph-thread-insert does the work of the traversal for the graph-> macro.
(defmulti graph-thread-insert
  (fn [form symb] (class form)))

; This is used to stop graph threading inside its application
(declare g-|)
(def graph-stop-threading-symb
  (symbol "g-|"))

; Any list beginning with a symbol that resolves to something in the core or protocol namespace is prefixed with the threaded symbol.
(defmethod graph-thread-insert clojure.lang.PersistentList [form symb]
  (with-meta
    (let [function (first form)
          rest-form (map #(graph-thread-insert % symb) (rest form))]
         (if (symbol? function)
             (if (= (resolve function) (resolve graph-stop-threading-symb))
                 `(do ~@(rest form))
                 (cons (first form)
                       (cons symb
                             (map #(graph-thread-insert % symb)
                                  (rest form)))))
             (cons (graph-thread-insert function symb) rest-form)))
    (meta form)))

; Multimethod dispatches on class, which requires me to duplicate for ArrayMap and HashMap, as both of these can show up in reader literals, depending on how large they are.
(defmethod graph-thread-insert clojure.lang.PersistentHashMap [form symb]
  (with-meta
    (zipmap (keys form) (map #(graph-thread-insert % symb) (vals form)))
    (meta form)))
(defmethod graph-thread-insert clojure.lang.PersistentArrayMap [form symb]
  (with-meta
    (zipmap (keys form) (map #(graph-thread-insert % symb) (vals form)))
    (meta form)))

; Vectors are traversed but not modified if they have no graph forms in them.
(defmethod graph-thread-insert clojure.lang.PersistentVector [form symb]
  (with-meta (vec (map #(graph-thread-insert % symb) form)) (meta form)))

; Sets are traversed but not modified if they have no graph forms in them.
(defmethod graph-thread-insert clojure.lang.PersistentHashSet [form symb]
  (with-meta (set (map #(graph-thread-insert % symb) form)) (meta form)))

; Anything not listed above is not traversed.
(defmethod graph-thread-insert :default [form symb]
  form)

(defmacro g->
  "Threads a graph through a series of computations, like ->, except the graph is also recursively inserted as the first argument to every graph-related function in any form. This establishes a local context so that queries like (node {:foo true}) need not reference the graph, even nested inside maps and vectors for other queries."
  ([x] x)
  ([x form]
   (if (seq? form)
       (let [let-symb (gensym)]
            `(let [~let-symb ~x] ; avoid re-computing operations using let statement
                  ~(graph-thread-insert form let-symb)))
       (list form x)))
  ([x form & more] `(g-> (g-> ~x ~form) ~@more)))

