(ns advent-of-code.utils
  (:require [clojure.data.priority-map :refer [priority-map]]))

(declare weighted-djkstra-walk*)

(defn weighted-djkstra-walk
  ([children-fn start]
   (weighted-djkstra-walk children-fn (priority-map start 0) #{}))
  ([children-fn nodes seen]
   (let [current (peek nodes)
         nodes   (pop nodes)]
     (when current
       (if (seen (first current))
         (recur children-fn nodes seen)
         (cons current
               (weighted-djkstra-walk* children-fn nodes seen current)))))))

(defn- weighted-djkstra-walk* [children-fn nodes seen [current-node steps]]
  (lazy-seq (weighted-djkstra-walk children-fn
                                   (into nodes
                                         (map #(update % 1 + steps))
                                         (children-fn current-node))
                                   (conj seen current-node))))

(defn djkstra-walk [children-fn start]
  (weighted-djkstra-walk (comp (partial map #(vector % 1)) children-fn) start))
