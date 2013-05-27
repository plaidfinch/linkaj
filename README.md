
linkaj
======

Persistent associative datatypes with more features than Clojure's built-in maps, including bijective maps, key-attribute-value maps, bipartite graphs, and directed graphs with arbitrary node and edge attributes.

## Invertible Maps

* The ``Invertible`` protocol has one method: ``inverse``, which returns the inverse of a datatype's mapping. The idea is that types implementing Invertible are designed so that ``inverse`` operates in O(1) time, as it does for all types given here.
* The ``Surjection`` type is a drop-in replacement for Clojure's ``hash-map`` datatype. It supports one additional method: ``inverse``, which returns a mapping from values to sets of keys which have them. This type returned, ``InvertedSurjection``, can then be inverted back into the original ``Surjection``.
* The ``Bijection`` type is much like Clojure's ``hash-map`` datatype, except it guarantees a bijective (1-to-1) mapping. This means that when performing ``assoc`` operations on a ``Bijection``, both keys and values may be overwritten with new mappings. A ``Bijection`` inverts into another ``Bijection``.
* The ``Bipartite`` type is an unrestricted mapping from keys to values. This means that keys can be associated with multiple values and values can be associated with multiple keys. In this way, it is a bipartite graph. It inverts into another ``Bipartite``.

## Attribute Maps

* The ``IAttributeMap`` protocol defines the core operations of an attribute map: a mapping where every key is associated with a set of pairs of attributes and values. ``AttributeMaps`` present themselves as maps from keys to maps from attributes to values, but internally, they are represented so as to provide fast (almost entirely O(1)) lookups for keys having particular attributes, attributes of a particular key, etc.

## Directed Graphs

* The ``IDirectedGraph``, ``Relational``, ``Constrained``, and ``IComponent`` protocols describe the functionality of a directed graph data structure, where every node and edge can have arbitrary attributes and values associated with it. This is perhaps not the fastest directed graph implementation out there, but it has a compelling syntax for manipulation, and is designed with the aim of encouraging more complex operations to be built onto it.

More documentation about directed graphs is forthcoming, but here are some samples of how it looks like to manipulate one:

```clojure
(-#> (digraph :relations [[:from :to]])
     (add-nodes {:id ['A 'B 'C 'D] :color [:red]})
     (add-cycle [:from :to]
                {:foo true :bar 500}
                (-#- (sort-by :id (nodes))))
     (nodes-away 2 :from (nodes {:id ['A]})))

;=> ({:color :red, :id C})
```

```clojure
(-#> (digraph :relations [[:parent :child]])
     (add-nodes {:id [1 2 3]})
     (add-edges {:parent (nodes {:id [2]})
                 :child  (nodes {:id [1 3]})})
     (assoc-edge (edge {:parent (node {:id [2]})
                        :child  (node {:id [3]})})
                 {:foo true})
     edges)

;=> ({:parent {:id 2}, :child {:id 1}} {:parent {:id 2}, :foo true, :child {:id 3}})
```

## License

Copyright © 2013 Kenneth Foner.
Licensing information to be determined.

