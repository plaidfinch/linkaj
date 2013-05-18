(ns map-experiments.directed-graph.protocol)

(defprotocol IDirectedGraph
  
  "Protocol for a directed graph."

  ; Methods acting on nodes:
  
  (nodes [graph attributes]
    "Returns all graph nodes matching the attributes.")

  (add-node [graph attributes]
    "Adds a node to the graph with attributes given.")
  
  (remove-node [graph attributes]
    "Removes a node from the graph with attributes given (if one exists). Returns an error if the attributes matches multiple nodes.")

  (remove-all-nodes [graph attributes]
    "Removes all possible nodes which have all the attributes given from the graph.")

  (assoc-node [graph attributes new-attributes]
    "Associates the node matching the attributes with the given attributes. Returns an error if attributes matches anything other than exactly one node.")

  (assoc-all-nodes [graph attributes new-attributes]
    "Associates any nodes matching the attributes with the given new attributes.")

  (dissoc-node [graph attributes attribute-keys]
    "Dissociates the node matching the attributes from the given attributes. Returns an error if attributes matches anything other than exactly one node.")

  (dissoc-all-nodes [graph attributes attribute-keys]
    "Dissociates any nodes matching the attributes from the given attributes.")
  
  ; Methods acting on edges:
  
  (edges [graph relations]
         [graph relations attributes]
    "Returns all graph edges matching relations and attributes.")
  
  (add-edge [graph relations]
            [graph relations attributes]
    "Adds an edge to the graph with a relation to nodes matching relations, giving it the specified attributes. Returns an error if the query matches multiple possible edges.")
  
  (add-all-edges [graph relations]
                 [graph relations attributes]
    "Adds all possible edges which could match relations to the graph, giving each the attributes given.")
  
  (remove-edge [graph relations]
               [graph relations attributes]
    "Removes an edge from the graph matching the query. Returns an error if the query matches multiple edges.")
  
  (remove-all-edges [graph relations]
                    [graph relations attributes]
    "Removes all possible edges which could match the query from the graph.")
  
  (move-edge [graph relations new-relations]
             [graph relations attributes new-relations]
    "Reassigns the edge specified by relations and attributes to have the new relations given. Returns an error if the query matches multiple edges.")
  
  (assoc-edge [graph relations new-attributes]
              [graph relations attributes new-attributes]
    "Associates the edge matching the query with the given attributes. Returns an error if query matches anything other than exactly one edge.")
  
  (assoc-all-edges [graph relations new-attributes]
                   [graph relations attributes new-attributes]
    "Associates any edges matching the query with the given new attributes.")
  
  (dissoc-edge [graph relations attribute-keys]
               [graph relations attributes attribute-keys]
    "Dissociates the edge matching the attributes from the given attributes. Returns an error if attributes matches anything other than exactly one edge.")
  
  (dissoc-all-edges [graph relations attribute-keys]
                    [graph relations attributes attribute-keys]
    "Dissociates any edges matching the attributes from the given attributes."))
