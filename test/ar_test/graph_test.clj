(ns ar-test.graph-test
  (:require [clojure.test :refer :all]
            [ar-test.graph :refer :all]))

(deftest test-create-first-node
  "Create a graph with one node in it"
  (is (=
        (add-node nil :1)
        {:1 nil})))

(deftest test-add-node-to-existing-graph
  "Add new node to an existing graph."
  (is (=
        (add-node {:1 nil} :2)
        {:1 nil :2 nil})))

(deftest test-add-existing-node-to-graph
  "Add a node to the graph that already exists in the graph.
   Must return unmodified graph."
  (is (=
        (add-node {:1 nil :2 nil} :2)
        {:1 nil :2 nil})))

(deftest test-connect-two-nodes-in-empty-graph
  "Add the given two nodes to a new graph, connect them and
  capture distance."
  (is (=
        (add-edge nil :1 :2 5)
        {:1 {:2 5} :2 nil})))

(deftest test-add-new-edge-between-existing-nodes
  "Given a graph with existing source node and target node
   that are not connected, add a new edge between them"
  (is (=
        (add-edge {:1 {:2 1} :2 {:3 5} :3 {:4 4} :4 nil} :1 :3 3)
        {:1 {:2 1 :3 3} :2 {:3 5} :3 {:4 4} :4 nil})))

(deftest test-update-distance-between-nodes
  "Given a graph with 2 nodes connected by a certain distance,
   update the distance between them with the new value given"
  (is (=
        (add-edge {:1 {:2 1} :2 {:3 5} :3 nil} :1 :2 3)
        {:1 {:2 3} :2 {:3 5} :3 nil})))

(deftest test-number-of-vertices-in-graph
  "Given a number of vertices and edges, checks whether random-graph function
  outputs a graph with the correct number of vertices"
  (is (=
        (->> (random-graph 5 10)
             (keys)
             (count))
        5)))

(deftest test-number-of-edges-in-graph
  "Given a number of vertices and edges, checks whether random-graph function
  outputs a graph with the correct number of edges"
  (is (=
        (->> (random-graph 5 10)
             (map second)
             (map count)
             (reduce +))
        10)))

(deftest test-neighbor-nodes-with-cumulative-weights
  "Given a traversed set of nodes at a vertex, and the neighbors of the vertex,
  checks if the map returned contains the neighboring nodes and cumulative distance
  from source"
  (let [graph {:1 {:2 2 :5 9}
              :2 {:3 1 :4 6 :5 3}
              :3 {:4 2}
              :4 {:5 1}}
        traversed-nodes {:1 [:1]}
        current-node :2
        current-cost 2]
    (is (=
          (explore-node graph traversed-nodes current-node current-cost)
          {:3 [3 :2], :4 [8 :2], :5 [5 :2]}))
    ))

(deftest test-shortest-path
  "Given a graph and 2 nodes, test if function D returns the shortest path
  between the two"
  (let [graph {:1 {:2 2 :5 9}
                     :2 {:3 1 :4 6 :5 3}
                     :3 {:4 2}
                     :4 {:5 1}}]
    (is (=
          (D graph :1 :5)
          [:1 :2 :5]))))