Future work:
- Constraint combinator library
   + Split constraints into a constraint predicate (i.e. "when creating a node") and a constraint action (i.e. "add the current time")
   + Each of these should be very parameterized and H-O
     * (on (add-node) (assoc-this-node :created now))
     * (on (remove-node (with-attr :name :root)) revert)
   + Need to define qualified new versions of common logical combinators, such as and, or, not, which are of type constraint -> constraint (-> constraint ...)
   + Factor out constraint composition into separate function, use it in graph for add-constraint
   + Library should be designed to be imported qualified, i.e. constraint/comp
- IO combinator library
   + generic graph writer to walk graph, takes functions to tell it how to format
   + DOT format writer

(defn now [] (java.util.Date.))

(def family-tree
(-#> (digraph :relations [[:parent :child] [:wife :husband]]
              :constraints
              [(fn [element action old-piece new-piece old-graph new-graph]
                (if (and (node? new-piece)
                         (not (:id new-piece))
                         (node-in? new-graph new-piece))
                    (assoc-node new-graph new-piece :id (rand-int Integer/MAX_VALUE))
                    new-graph))
               (fn [element action old-piece new-piece old-graph new-graph]
                 (if (= action :add)
                     (case element
                       :node (assoc-node new-graph new-piece :created (now))
                       :edge (assoc-edge new-graph new-piece :created (now)))
                     new-graph))
               (fn [element action old-piece new-piece old-graph new-graph]
                 (if (and (or (= action :assoc) (= action :dissoc))
                          (not (:modified new-piece)))
                     (case element
                       :node (assoc-node new-graph new-piece :modified (now))
                       :edge (assoc-edge new-graph new-piece :modified (now)))
                     new-graph))])
     (add-nodes :name ["Nancy" "Harvey" "Brenda" "Si" "Debby" "Joel" "Kenny" "Danny"])
     (add-edge :parent (node :name "Nancy") :child (node :name "Debby"))
     (add-edge :parent (node :name "Harvey") :child (node :name "Debby"))
     (add-edge :parent (node :name "Brenda") :child (node :name "Joel"))
     (add-edge :parent (node :name "Si") :child (node :name "Joel"))
     (add-edge :parent (node :name "Debby") :child (node :name "Kenny"))
     (add-edge :parent (node :name "Joel") :child (node :name "Kenny"))
     (add-edge :parent (node :name "Debby") :child (node :name "Danny"))
     (add-edge :parent (node :name "Joel") :child (node :name "Danny"))
     (add-edge :husband (node :name "Harvey") :wife (node :name "Nancy"))
     (add-edge :husband (node :name "Si") :wife (node :name "Brenda"))))

(-#> (digraph :relations [[:from :to]])
     (add-nodes :id ['A 'B 'C 'D])
     (add-edges :from (node :id 'B) :to (nodes :id ['C 'D]))
     (add-edge :from (node :id 'A) :to (node :id 'B)))

(-#> (digraph :relations [[:parent :child]])
     (add-nodes :id [1 2 3])
     (add-edges :parent (node :id 2)
                :child  (nodes :id [1 3]))
     (assoc-edge (edge :parent (node :id 2)
                       :child  (node :id 3))
                 :foo true))

Thoughts about constraints:
- Prevent stack overflow by checking to see if a condition is satisfied before invoking a graph modification method. *We* know that assoc adds an attribute if it doesn't exist yet, and does nothing if it already exists, but the graph doesn't. Unrestricted use of modification procedures will almost always lead to stack overflow for this reason.
- Checking whether a node given as argument is present in either of the two graphs is a good idea, especially in the case of removal. Careless coding of constraints can lead to errors when trying to act on a constraint during a graph element removal step.
- A pattern for throwing assertions through constraints:
  + (fn [old-graph new-graph x]
        (assert something-that-should-be-true "That thing isn't true!")
        new-graph)
- Another useful pattern is that where disallowed changes are simply prevented without throwing an error:
  + (fn [old-graph new-graph x]
        (if something-not-allowed
            old-graph
            new-graph))
- Things to do with HO constraints
  + disallow modification of a particular attribute once it's created
  + building on that, automatic creation / modification timestamps which cannot be altered without legitimately creating / modifying
  + automatic unique id for each element, guaranteed not to clash
  + force graph to be a binary tree, throwing an exception if condition is violated
- Complex constraint composition issues can occur if there is improper separation of concerns. If two constraints both try to act on the same attribute, here be dragons.

  (-#> (digraph :relations [[:from :to]]) (add-nodes :id [1 2 3 4]) (add-cycle [:from :to] (-#- (sort-by :id (nodes)))))

(let [g (time (-#> (digraph :relations [[:from :to]])
                   (add-nodes :id (range 25000))
                   (add-cycle [:from :to] (nodes))))]
  (time (dotimes [_ 1e5] (-#> g (assoc-node (node :id 1) :foo :bar)))))

; This manual loop with transient collections is about 15% faster than the below higher-order lazy-sequence manipulation (which still uses transients under the hood in `into'). Usually, the slight performance increase isn't worth the code bloat.
(def unicode
(time
  (loop [n 0
         u (transient (bijection))]
   (if (< n 65535)
    (recur (inc n) (assoc! u n (char n)))
    (persistent! u)))))

(def unicode
  (time
    (into (bijection)
          (map (juxt identity char)
               (range 65535)))))
