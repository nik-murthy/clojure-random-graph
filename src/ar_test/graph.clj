(ns ar-test.graph
  (require [clojure.data.priority-map :refer [priority-map]]))

;seq for assigning weights (or distances) to edges in graph
(def weights (vec (range 1 50)))

(defn add-node
  "Given a node and a graph, adds the node to it. Works for empty graph.
   Here is an example graph -
   {:1 {:2 4 :3 6}
    :2 {:3 1}
    :3 nil }
    Each node is a key in the graph and the adjacency list of the node, as a map, is the value."
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

(defn get-possible-target-nodes
  "Given a graph and node in the graph, returns all the possible nodes that given
  node can be connected to"
  [graph node]
  (->> (keys graph)
       (remove (set (list node)))
       (remove (if (node graph)
                 (node graph)
                 (set nil)))))

(defn get-random-node
  "Given a graph, retrieves a random node. Graph must contain at least one node."
  [graph]
  (->> (keys graph)
       (vec)
       (rand-nth)))

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
  [graph num-edges]
  (loop [finished-graph graph]
    (if (= num-edges (->> finished-graph
                          (map second)
                          (map count)
                          (reduce +)))
      finished-graph
      (recur
        (random-add-new-edge finished-graph (get-random-node graph))))))

(defn random-graph
  "Given number of nodes (or vertices) and number of edges, generates a random simple
  connected graph"
  [num-vertices num-edges]
  (let [vertices-coll (->> (range 1 (+ num-vertices 1))
                           (map str)
                           (map keyword))
        max-edges (quot (* num-vertices (- num-vertices 1)) 2)
        initial-graph (add-node {} (first vertices-coll))
        graph-with-vertices (add-all-vertices initial-graph (rest vertices-coll))]
    (cond
      (> num-edges max-edges)
      (prn "Not a valid graph. Number of edges given is greater than n(n-1)/2 where n is number of nodes.")

      (< num-edges (- num-vertices 1))
      (prn "Not a valid graph. Number of edges should be greater than or equal to number of nodes.")

      :else
      (add-all-edges graph-with-vertices num-edges))))

(defn explore-node
  "Given a graph, a node to be explored, a map of previously explored nodes, and
  the current cost at the given node, returns the neighbors with the cumulative cost
  from the source
  Test case: get-neighbor-nodes-with-cumulative-weights"
  [graph visited-nodes current-node current-cost]
  (let [unexplored-neighbors (->> (current-node graph)
                                  (remove visited-nodes))]
    (into {} (for [node-neighbor unexplored-neighbors]
               [(first node-neighbor) [(+ current-cost
                                          (get-in graph [current-node (first node-neighbor)]))
                                       current-node]]))))

(defn get-all-paths
  "Given a graph and a source node, finds the path to all the other nodes
  in the graph. The resulting data structure looks as following -
  ([:1 0 [:1]] [:2 3[:1]])
  The first value is the destination node, followed by weight/distance and final vector
  is the path from the source node (the shortest path to :2 is from :1
  in this example)"
  [graph source-node]
  ((fn traverse-graph
     [visited-nodes next-nodes]
     (lazy-seq
       (when-let [[current-node [total-cost previous-node]] (peek next-nodes)]
         (let [path (conj (vec (visited-nodes previous-node)) current-node)
               visited-nodes (assoc visited-nodes current-node path)
               neighbor-nodes (explore-node graph visited-nodes current-node total-cost)
               next-nodes (merge-with min-key
                                      (pop next-nodes)
                                      neighbor-nodes)]
           (cons [current-node total-cost path]
                 (traverse-graph visited-nodes next-nodes))))))
   {}
   (priority-map source-node [0])))

(defn D
  "Given a graph, a source node and a destination node, returns the list of nodes
  which forms the shortest path between the source and destination nodes"
  [graph source-node destination-node]
  (let [not-destination? (fn [[vertex _]]
                           (not= vertex destination-node))
        all-paths (get-all-paths graph source-node)
        shortest-path (-> (drop-while not-destination? all-paths)
                          (first)
                          (nth 2))]
    (if (nil? shortest-path)
      (prn "No path.")
      shortest-path)
    ))

(comment
  (let [graph {:1 {:2 2 :5 9}
               :2 {:3 1 :4 6 :5 3}
               :3 {:4 2}
               :4 {:5 1}}
        traversed-nodes {:1 [:1]}
        current-node :2
        current-cost 2
        unexplored-nodes (->> (current-node graph)
                              (remove traversed-nodes))]
    (explore-node graph traversed-nodes current-node current-cost))

  (peek (priority-map :1 [0]))

  (let [graph {:1 {:2 2 :5 9}
               :2 {:3 1 :4 6 :5 3}
               :3 {:4 2}
               :4 {:5 1}}
        next-nodes (priority-map :1 [0])
        visited-nodes {}
        [current-node [total-cost previous-node]] (peek next-nodes)
        path (conj (vec (visited-nodes previous-node)) current-node)
        visited-nodes (assoc visited-nodes current-node path)
        neighbor-nodes (explore-node graph visited-nodes current-node total-cost)
        next-nodes (merge-with min-key
                               (pop next-nodes)
                               neighbor-nodes)]
    next-nodes)

  (let [graph {:1 {:2 10, :3 49, :4 16}
               :2 {:3 10}
               :3 {:5 24}
               :4 nil
               :5 nil}]
    (D graph :4 :5))

  (let [graph {:1 {:2 2 :5 9}
               :2 {:3 1 :4 6 :5 3}
               :3 {:4 2}
               :4 {:5 1}}]
    (get-all-paths graph :1))

  (let [graph (random-graph 5 10)]
    (D graph :1 :4))

  (random-graph 5 10)

  (let [graph {:1 {:2 8, :3 31, :4 12}, :2 nil, :3 {:4 40}, :4 nil}]
    (->> graph
         (map second)
         (map count)
         (reduce +)))

  (let [graph {:1 {:2 8, :3 31, :4 12}, :2 nil, :3 {:4 40}, :4 nil}]
    (->> graph
         (keys)
         (count)))
  )


