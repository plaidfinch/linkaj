(ns map-experiments.directed-graph.protocol)

(defprotocol IComponent
  "Protocol for a component of a larger data structure."
  (id [this] "Returns the id of the component"))

(defprotocol Relational
  "Protocol for an object with relations and opposites which can be added or removed." 
  (relations       [o]       "Returns a bijection between all relations.")
  (related-in?     [o r1 r2] "Tells if two relations are related in the object.")
  (relation-in?    [o r]     "Tells if something is a relation in the object.")
  (add-relation    [o r1 r2] "Adds the relation r1 & r2 to the object.")
  (remove-relation [o r1 r2] "Removes the specified relation from the object."))

(defprotocol Constrained
  "Protocol for an object with arbitrary constraints which can be added or removed."
  (add-constraint [o f]
    "Adds a constraint function f to the object o. Future alterations to the object will be handed to the constraint function to verify and modify.")
  (reset-constraints [o]
    "Removes all constraints from the object.")
  (verify-constraints [o]
    "Checks every constraint against every part of the object. Useful only in the rare situation when constraints are added to post-construction and are desired to be backwards-checked against the existing object."))

(defprotocol IDirectedGraph
  "Protocol for a directed graph."
  
  ; Methods acting on nodes:
  (nodes [graph] [graph query]
    "Returns all graph nodes matching the query.")
  (node? [graph x]
    "Returns true if x is a node key in the graph.")
  (get-node [graph node-key]
    "Returns a map of all attributes for node-key.")
  (add-node [graph attributes]
    "Adds a node with attributes to the graph.")
  (remove-node [graph node-key]
    "Removes node-key from the graph, as well as all edges which are directly connected to this node.")
  (assoc-node [graph node-key attributes]
    "Associates node-key with attributes.")
  (dissoc-node [graph node-key attribute-keys]
    "Dissociates node-key from attribute-keys.")
  
  ; Methods acting on edges:
  (edges [graph] [graph query]
    "Returns all graph edges matching the query.")
  (edge? [graph x]
    "Returns true if x is an edge in the graph.")
  (get-edge [graph edge-key]
    "Returns a map of all attributes for edge-key.")
  (add-edge [graph attributes]
    "Adds an edge with attributes to the graph. Attributes must contain exactly two relations, and they must be each others' opposites.")
  (remove-edge [graph edge-key]
    "Removes edge-key from the graph.")
  (assoc-edge [graph edge-key attributes]
    "Associates edge-key with attributes. This cannot change relations.")
  (move-edge [graph edge-key relations]
    "Moves edge to have relations. This cannot change which relations there are, only the nodes to which the edge is related.")
  (dissoc-edge [graph edge-key attribute-keys]
    "Dissociates edge-key from attribute-keys. Relations cannot be dissociated."))

