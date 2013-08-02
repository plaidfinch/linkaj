(ns map-experiments.directed-graph.macro)

; Special threading macro to allow automatic context for queries and threading through multiple operations to boot.

(defn insert-at
  "Inserts something between the nth and n+1th elements of a seq."
  [n x coll]
  (concat (take n coll) [x] (drop n coll)))

; Quasi-macros used for changing threading inside the -#> form:
(declare -#| -#-)
(def stop-threading-symb
  (symbol "-#|"))
(def skip-threading-symb
  (symbol "-#-"))
(def ensure-threading-symb
  (symbol "-#+"))

; Graph-thread-insert does the work of the traversal for the -#> macro.
(defmulti graph-thread-insert
  (fn [form symb] (class form)))

; Any list beginning with a symbol that resolves to something in the core or protocol namespace is prefixed with the threaded symbol.
(defmethod graph-thread-insert clojure.lang.PersistentList [form symb]
  (with-meta
    (let [function (first form)
          rest-form (map #(graph-thread-insert % symb) (rest form))
          rest-form-skip (map (fn [x] (map #(graph-thread-insert % symb) x))
                              (rest form))
          rest-form-stop (rest form)]
         (if (symbol? function)
             (cond (= function 'quote) form
                   (= (resolve function) (ns-resolve *ns* stop-threading-symb))
                   `(do ~@rest-form-stop)
                   (= (resolve function) (ns-resolve *ns* skip-threading-symb))
                   `(do ~@rest-form-skip)
                   (= (resolve function) (ns-resolve *ns* ensure-threading-symb))
                   `(~@(first rest-form) ~symb ~@(rest rest-form))
                   :else
                   (if-let [pos-fn (::thread-position-fn (meta (resolve function)))]
                           (cons function
                                 (insert-at (pos-fn (count rest-form-stop))
                                            symb
                                            rest-form))
                           (cons function rest-form)))
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

(defmacro -#>
  "Threads a graph through a series of computations, like ->, except the graph is also recursively inserted as the first argument to every function in any form. This establishes a local context so that queries like (node :foo true) need not reference the graph, even nested inside maps and vectors for other queries."
  ([x] x)
  ([x form]
   (if (seq? form)
       (let [let-symb (gensym)]
            `(let [~let-symb ~x] ; avoid re-computing operations using let statement
                  ~(graph-thread-insert form let-symb)))
       (list form x)))
  ([x form & more] `(-#> (-#> ~x ~form) ~@more)))

(defmacro defgraphfn
  "Defines a new graph function by automatically noting that the graph is the first argument. For more complex insertion behavior, use declare-graph-fn."
  [name & decls]
  (list* `defn
         (with-meta name (assoc (meta name) ::thread-position-fn (constantly 0)))
         decls))

(defmacro declare-graph-fn
  "Explicitly designates a function as graph-thread (-#>) capable. This is for use when either a) the function takes the graph as input in an argument other than its first (rendering defgraphfn inadequate) or b) the function was defined in a place outside the programmer's control, but the programmer wishes the -#> macro to thread a graph into this function.
  
  Takes a name to affect, and a function from the number of arguments the graph function is called with (sans graph) to the position at which to insert the graph in the function call.
  
  Note that this must be called after a function is defined via def or defn, as it has to be able to alter the metadata on the function, and declaring, setting metadata, and then defining wipes the metadata that was set before the definition."
  [name function]
  ;`(declare ~function)
  `(alter-meta! (resolve '~name) #(conj % {::thread-position-fn ~function})))

(defmacro declare-graph-fns
  "Takes a list of functions, and declares them all as graph functions with the graph as the first argument."
  [& functions]
  `(do ~@(map (fn [f] (list `declare-graph-fn f `(constantly 0))) functions)))
