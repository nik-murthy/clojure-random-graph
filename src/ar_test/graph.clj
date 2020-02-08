(ns ar-test.graph)

(def weights (vec (range 1 50)))

(defn add-node
  "This method adds a node to given graph. Use empty map to create a graph with one node.
   The graph is represented as a map with each vertex as key and distance to connected
   vertices as value. The value is another map where key is connected vertex and value is
   the distance. Here is an example -
   {:1 {:2 4 :3 6}
    :2 {:3 1}
    :3 nil } "
  [graph node-name]
  (cond (get graph node-name) graph
        :else (assoc graph node-name nil)))

(defn add-edge
  "This method takes 2 nodes and the distance between them and adds them to the graph.
   Given an empty graph, the method adds the two vertices to the graph and records the
   distance between them."
  [graph source-node target-node distance]
  (assoc
    (add-node graph target-node)
    source-node
    (-> (get graph source-node)
        (assoc target-node distance))))

(defn connect-random-vertices
  [graph vertex]
  (let [added-nodes   (vec (keys graph))
        source-node   (rand-nth added-nodes)]
    (add-edge graph source-node vertex (rand-nth weights))))

(defn add-all-vertices
  [graph vertices-coll]
  (loop [vertices vertices-coll
         new-graph graph]
    (if (empty? vertices)
      new-graph
      (recur (rest vertices) (connect-random-vertices new-graph (first vertices))))))


(comment

  (let [num-vertices 10
        vertices-coll (->> (range 1 (+ num-vertices 1))
                           (map str)
                           (map keyword))
        initial-graph (add-node {} (first vertices-coll))]
    (map connect-vertices initial-graph (rest vertices-coll)))

  (let [num-vertices 10
        vertices-coll       (->> (range 1 (+ num-vertices 1))
                              (map str)
                              (map keyword))
        initial-graph       (add-node {} (first vertices-coll))
        graph-with-vertices (add-all-vertices initial-graph (rest vertices-coll))]
    graph-with-vertices
    )

  (for [vertex (rest '(:2 :3))]
    (connect-vertices {:1 nil} vertex))

  (loop [vertex       '(:2 :3 :4)
         initial-map  {:1 nil}]
    (if (empty? vertex)
    initial-map
    (recur (rest vertex) (connect-random-vertices initial-map (first vertex)))))

  (doseq [vertex (rest '(:2 :3))]
    (connect-vertices {:1 nil} vertex))

  (connect-vertices {:1 nil} :2)
  (connect-vertices {} :2)

  (map #(connect-vertices {:1 nil} %) '(:2 :3))

  (map #(connect-vertices {} %) '(:2 :3))
  )

