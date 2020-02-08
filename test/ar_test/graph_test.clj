(ns ar-test.graph-test
  (:require [clojure.test :refer :all]
            [ar-test.graph :refer :all]))

(deftest create-first-node
  "Create a graph with one node in it"
  (is (=
        (add-node nil :1)
        {:1 nil})))

(deftest add-node-to-existing-graph
  "Add new node to an existing graph."
  (is (=
        (add-node {:1 nil} :2)
        {:1 nil :2 nil})))

(deftest add-existing-node-to-graph
  "Add a node to the graph that already exists in the graph.
   Must return unmodified graph."
  (is (=
        (add-node {:1 nil :2 nil} :2)
        {:1 nil :2 nil})))

(deftest connect-two-nodes-in-empty-graph
  "Add the given two nodes to a new graph, connect them and
  capture distance."
  (is (=
        (add-edge nil :1 :2 5)
        {:1 {:2 5} :2 nil})))

(deftest add-new-edge-between-existing-nodes
  "Given a graph with existing source node and target node
   that are not connected, add a new edge between them"
  (is (=
        (add-edge {:1 {:2 1} :2 {:3 5} :3 {:4 4} :4 nil} :1 :3 3)
        {:1 {:2 1 :3 3} :2 {:3 5} :3 {:4 4} :4 nil})))

(deftest update-distance-between-nodes
  "Given a graph with 2 nodes connected by a certain distance,
   update the distance between them with the new value given"
  (is (=
        (add-edge {:1 {:2 1} :2 {:3 5} :3 nil} :1 :2 3)
        {:1 {:2 3} :2 {:3 5} :3 nil})))
