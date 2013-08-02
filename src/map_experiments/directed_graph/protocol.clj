(ns map-experiments.directed-graph.protocol
  (:require [map-experiments.directed-graph.macro :refer :all]))

(defprotocol IComponent
  "Protocol for a component of a larger data structure."
  (id [this] "Returns the id of the component. Do not use this function in client code; it is meant to be for implementation-specific internal use only."))

(defprotocol Relational
  "Protocol for an object with relations and opposites which can be added or removed." 
  (relations       [o]       "Returns a set of sets of relations.")
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
    "Checks every constraint against every part of the object. Useful only in the rare situation when constraints are added to post-construction and are desired to be backwards-checked against the existing object.")
  (constraints [o]
    "Returns the constraint function currently in the object."))

(defprotocol IDirectedGraph
  "Protocol for a directed graph."
  
  ; Methods acting on nodes:
  (nodes* [graph] [graph query]
    "Returns all graph nodes matching the query.")
  (node-in? [graph x]
    "Returns true if x is a node key in the graph.")
  (add-node* [graph attributes]
    "Adds a node with attributes to the graph.")
  (remove-node [graph n]
    "Removes node n from the graph, as well as all edges which are directly connected to this node.")
  (assoc-node* [graph n attributes]
    "Associates node n with attributes.")
  (dissoc-node* [graph n attribute-keys]
    "Dissociates node n from attribute-keys.")
  
  ; Methods acting on edges:
  (edges* [graph] [graph query]
    "Returns all graph edges matching the query.")
  (edge-in? [graph x]
    "Returns true if x is an edge in the graph.")
  (add-edge* [graph attributes]
    "Adds an edge with attributes to the graph. Attributes must contain exactly two relations, and they must be each others' opposites.")
  (remove-edge [graph e]
    "Removes edge-key from the graph.")
  (assoc-edge* [graph e attributes]
    "Associates edge-key with attributes. This can change relations.")
  (dissoc-edge* [graph e attribute-keys]
    "Dissociates edge-key from attribute-keys. Relations cannot be dissociated."))

; All of the functions listed below are functions with a graph as their first argument; as such, they participate in the -#> macro and are auto-threaded.
(declare-graph-fns-0
  relations related-in? relation-in? add-relation remove-relation
  add-constraint reset-constraints verify-constraints constraints
  nodes* node-in? add-node* remove-node assoc-node* dissoc-node*
  edges* edge-in? add-edge* remove-edge assoc-edge* dissoc-edge*)
