(ns ar-test.graph)

(def weights (vec (range 1 50)))

(defn get-random-node
  [graph]
  (->> (keys graph)
       (vec)
       (rand-nth)))

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

(defn random-add-new-vertex
  [graph vertex]
  (let [source-node (get-random-node graph)]
    (add-edge graph source-node vertex (rand-nth weights))))


(defn add-all-vertices
  [graph vertices-coll]
  (loop [vertices vertices-coll
         new-graph graph]
    (if (empty? vertices)
      new-graph
      (recur (rest vertices) (random-add-new-vertex
                               new-graph (first vertices))))))


(defn get-possible-target-nodes
  [graph node]
  (->> (keys graph)
       (remove (set (list node)))
       (remove (if (node graph)
                 (node graph)
                 (set '())))))

(defn already-connected?
  [graph target-node current-node]
  (->> graph
       (target-node)
       (current-node)))

(defn remove-already-connected
  [graph possible-target-nodes node]
  (loop [nodes possible-target-nodes
         unconnected-nodes '()]
    (if (empty? nodes)
      unconnected-nodes
      (recur
        (rest nodes)
        (if (already-connected?
              graph
              (first nodes)
              node)
          unconnected-nodes
          (cons (first nodes) unconnected-nodes)))
      )))

(defn random-add-new-edge
  [graph node]
  (let [possible-target-nodes (get-possible-target-nodes graph node)
        target-node (->> node
                         (remove-already-connected
                           graph possible-target-nodes)
                         (vec)
                         (rand-nth))]
    (add-edge graph node target-node (rand-nth weights))))

(defn add-all-edges
  [graph additional-edges]
  (loop [count additional-edges
         finished-graph graph]
    (if (= 0 count)
      finished-graph
      (recur
        (- count 1)
        (random-add-new-edge finished-graph (get-random-node graph))))))

(defn create-graph
  [num-vertices num-edges]
  (let [vertices-coll (->> (range 1 (+ num-vertices 1))
                           (map str)
                           (map keyword))
        additional-edges (- num-edges num-vertices)
        initial-graph (add-node {} (first vertices-coll))
        graph-with-vertices (add-all-vertices initial-graph (rest vertices-coll))]
    (if (> num-edges num-vertices)
      (add-all-edges graph-with-vertices additional-edges)
      (if (= num-edges num-vertices)
        graph-with-vertices
        (prn "Number of edges should be greater than or equal to number of vertices")))))

(comment

  (let [num-vertices 10
        vertices-coll (->> (range 1 (+ num-vertices 1))
                           (map str)
                           (map keyword))
        initial-graph (add-node {} (first vertices-coll))
        graph-with-vertices (add-all-vertices initial-graph (rest vertices-coll))]
    graph-with-vertices
    )


  (loop [vertex '(:2 :3 :4)
         initial-map {:1 nil}]
    (if (empty? vertex)
      initial-map
      (recur (rest vertex) (random-add-new-vertex initial-map (first vertex)))))


  (let [graph {:10 nil,
               :4  {:6 28},
               :7  nil,
               :1  {:2 5, :4 45},
               :8  nil,
               :9  nil,
               :2  {:3 32, :5 14, :9 21},
               :5  {:7 17, :8 4, :10 6},
               :3  nil,
               :6  nil}
        random-node (get-random-node graph)
        possible-target-nodes (get-possible-target-nodes graph random-node)
        ]
    (remove-already-connected
      graph
      possible-target-nodes
      random-node))

  (let [graph {:10 nil,
               :4  {:6 28},
               :7  nil,
               :1  {:2 5, :4 45},
               :8  nil,
               :9  nil,
               :2  {:3 32, :5 14, :9 21},
               :5  {:7 17, :8 4, :10 6},
               :3  nil,
               :6  nil}
        node :4
        possible-target-nodes (get-possible-target-nodes graph node)
        target-node (->> node
                         (remove-already-connected
                           graph possible-target-nodes)
                         (vec)
                         (rand-nth))
        new-graph (random-add-new-edge graph node)]
    ;(get-possible-target-nodes graph node)
    ;(remove-already-connected graph possible-target-nodes node)
    ;(get-random-node graph))
    ;target-node
    new-graph)

  (let [n 5
        e 10]
    (create-graph n e))

  )

