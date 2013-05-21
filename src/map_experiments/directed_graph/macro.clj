(ns map-experiments.directed-graph.macro)

; Special threading macro graph-> to allow automatic context for queries and threading through multiple operations to boot.

; Graph-thread-insert does the work of the traversal for the graph-> macro.
(defmulti graph-thread-insert
  (fn [form symb] (class form)))

; This is used to stop graph threading inside its application
(declare -#|)
(def graph-stop-threading-symb
  (symbol "-#|"))

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

(defmacro -#>
  "Threads a graph through a series of computations, like ->, except the graph is also recursively inserted as the first argument to every graph-related function in any form. This establishes a local context so that queries like (node {:foo true}) need not reference the graph, even nested inside maps and vectors for other queries."
  ([x] x)
  ([x form]
   (if (seq? form)
       (let [let-symb (gensym)]
            `(let [~let-symb ~x] ; avoid re-computing operations using let statement
                  ~(graph-thread-insert form let-symb)))
       (list form x)))
  ([x form & more] `(g-> (g-> ~x ~form) ~@more)))
