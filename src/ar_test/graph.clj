(ns ar-test.graph)

;seq for assigning weights (or distances) to edges in graph
(def weights (vec (range 1 50)))

;Using positive infinity as starting point of shortest path calculation
(def ^:private inf Double/POSITIVE_INFINITY)

(defn get-random-node
  "Given a graph, retrieves a random node. Graph must contain at least one node."
  [graph]
  (->> (keys graph)
       (vec)
       (rand-nth)))

(defn add-node
  "Given a node and a graph, adds the node to it. Works for empty graph.
  Here is an example graph -
   {:1 {:2 4 :3 6}
    :2 {:3 1}
    :3 nil } "
  [graph node-name]
  (cond (get graph node-name) graph
        :else (assoc graph node-name nil)))

(defn add-edge
  "Given a graph, 2 nodes and distance, adds edge between nodes.
   Given an empty graph, add two vertices to graph with distance."
  [graph source-node target-node distance]
  (assoc
    (add-node graph target-node)
    source-node
    (-> (get graph source-node)
        (assoc target-node distance))))

(defn random-add-new-vertex
  "Given a node, pulls a random existing node from given graph and
  adds an edge with a random weight assigned to the edge"
  [graph vertex]
  (let [source-node (get-random-node graph)]
    (add-edge graph source-node vertex (rand-nth weights))))


(defn add-all-vertices
  "Given collection of nodes(or vertices), adds all of them to an existing graph"
  [graph vertices-coll]
  (loop [vertices vertices-coll
         new-graph graph]
    (if (empty? vertices)
      new-graph
      (recur (rest vertices) (random-add-new-vertex
                               new-graph (first vertices))))))


(defn get-possible-target-nodes
  "Given a graph and node in the graph, returns all the possible nodes that given
  node can be connected to"
  [graph node]
  (->> (keys graph)
       (remove (set (list node)))
       (remove (if (node graph)
                 (node graph)
                 (set nil)))))

(defn already-connected?
  "Given a graph and 2 nodes in it, returns the weight of edge between them,
  returns nil otherwise"
  [graph target-node current-node]
  (->> graph
       (target-node)
       (current-node)))

(defn remove-already-connected
  "Given a graph, a node in it and possible nodes the given node can be connected to,
  checks if the node is already connected to any of the possible nodes"
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
  "Given a graph and a node, adds an edge between given node and a random node from the
  graph that given node is not yet connected to"
  [graph node]
  (let [possible-target-nodes (get-possible-target-nodes graph node)
        unconnected-target-nodes (remove-already-connected
                                   graph possible-target-nodes node)]
    (if (not-empty unconnected-target-nodes)
      (add-edge graph node (rand-nth (vec unconnected-target-nodes)) (rand-nth weights))
      graph)))

(defn add-all-edges
  "Given a graph where all nodes (or vertices) have been added and connects nodes not
  yet connected until the number of edges required is complete"
  [graph additional-edges]
  (loop [count additional-edges
         finished-graph graph]
    (if (= 0 count)
      finished-graph
      (recur
        (- count 1)
        (random-add-new-edge finished-graph (get-random-node graph))))))

(defn random-graph
  "Given number of nodes (or vertices) and number of edges, generates a random simple
  connected graph"
  [num-vertices num-edges]
  (let [vertices-coll (->> (range 1 (+ num-vertices 1))
                           (map str)
                           (map keyword))
        max-edges (quot (* num-vertices (- num-vertices 1)) 2)
        additional-edges (- (+ num-edges 1) num-vertices)
        initial-graph (add-node {} (first vertices-coll))
        graph-with-vertices (add-all-vertices initial-graph (rest vertices-coll))]
    (cond
      (> num-edges max-edges)
      (prn "Not a valid graph. Number of edges given is greater than n(n-1)/2 where n is number of nodes.")

      (< num-edges (- num-vertices 1))
      (prn "Not a valid graph. Number of edges should be greater than or equal to number of nodes.")

      :else
      (add-all-edges graph-with-vertices additional-edges))))

(defn D
  "Given a graph, a source node and a destination node, returns the list of nodes
  which forms the shortest path between the source and destination nodes"
  [graph source-node destination-node]
  )

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
    new-graph)

  (let [num-edges 5
        num-vertices 11]
    (> num-vertices (quot (* num-edges (- num-edges 1)) 2)))

  (let [n 5
        e 4]
    (random-graph n e))

  )


