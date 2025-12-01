(ns advent-of-code.utils
  (:require [advent-of-code.grid :as grid]
            [clojure.data.priority-map :refer [priority-map]]))

(defn- a*-queue [hueristic-fn [nodes breadcrumbs] [child-node steps] node]
  (let [[current-steps] (breadcrumbs child-node)]
    (cond
      (or (nil? current-steps)
          (> current-steps steps))
      (let [{::keys [edge]} (meta child-node)
            child-node (vary-meta child-node dissoc ::edge)]
        [(assoc nodes child-node [(+ steps (hueristic-fn child-node)) steps])
       (assoc breadcrumbs child-node [steps
                                      [(vary-meta node assoc ::edge edge)]])])

      (< current-steps steps)
      [nodes breadcrumbs]

      (= current-steps steps)
      [nodes
       (update-in breadcrumbs
                  [child-node 1]
                  conj
                  (vary-meta node assoc ::edge (::edge (meta child-node))))])))

(defn- paths [breadcrumbs node]
  (sequence (comp (remove (fn [[head]] (seq (nth (breadcrumbs head) 1))))
                  (map vec))
            (tree-seq (constantly true)
                    (fn [[node :as path]]
                      (map #(cons % path) (nth (breadcrumbs node) 1)))
                    (list node))))

(defn a*
  ([children-fn hueristic-fn start]
   (a* children-fn
       hueristic-fn
       (priority-map start [(hueristic-fn start) 0])
       #{}
       {start [0 []]}))
  ([children-fn hueristic-fn nodes seen breadcrumbs]
   (when-some [[node [_ steps]] (peek nodes)]
     (let [nodes (pop nodes)]
       (if (seen node)
         (recur children-fn hueristic-fn nodes seen breadcrumbs)
         (let [[nodes breadcrumbs]
               (transduce (comp (filter some?) (map #(update % 1 + steps)))
                          (completing #(a*-queue hueristic-fn %1 %2 node))
                          [nodes breadcrumbs]
                          (children-fn node))]
           (cons [node steps (paths breadcrumbs node)]
                 (lazy-seq (a* children-fn
                               hueristic-fn
                               nodes
                               (conj seen node)
                               breadcrumbs)))))))))

(defn weighted-djkstra-walk [children-fn start]
  (a* children-fn (constantly 0) start))

(def parse-2d-map grid/parse)

(defn djkstra-walk [children-fn start]
  (weighted-djkstra-walk (comp (partial map #(vector % 1)) children-fn) start))
