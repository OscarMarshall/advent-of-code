(ns advent-of-code.year-2023.day-23
  (:require [advent-of-code.core :as core]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)

(core/set-date! 2023 23)


;;;; Parse

(defn parse-input [input] (mapv vec (str/split-lines input)))

(core/set-parse-fn! parse-input)

;;;; Part 1

(defn all-locations [chart]
  (map vec (combo/cartesian-product (range (count chart))
                                    (range (count (first chart))))))

(def neighbor-vectors [[-1 0] [0 1] [1 0] [0 -1]])

(def path-tiles #{\.})
(def slope-tiles #{\^ \> \v \<})
(def walkable-tiles (into path-tiles slope-tiles))

(defn neighbors [location chart]
  (filter #(walkable-tiles (get-in chart %))
          (map #(mapv + location %) neighbor-vectors)))

(defn node? [location chart]
  (and (= (get-in chart location) \.)
       (> (count (filter #(walkable-tiles (get-in chart %))
                         (map #(mapv + location %) neighbor-vectors)))
          2)))

(def vector->slope {[-1 0] \^,[0 1] \>, [1 0] \v, [0 -1] \<})

(defn next-node [from to chart nodes]
  (loop [from from, to to, n 1]
    (let [vector (mapv - to from)]
      (when ((conj path-tiles (vector->slope vector)) (get-in chart to))
        (if (nodes to)
          [to n]
          (recur to (first (remove #{from} (neighbors to chart))) (inc n)))))))

(defn node-edges [location chart]
  (neighbors location chart))

(defn analyze-chart [chart next-node]
  (let [start [0 1]
        end   [(dec (count chart)) (- (count (first chart)) 2)]
        nodes (into #{start end}
                    (filter #(node? % chart))
                    (all-locations chart))
        edges (into {}
                    (map (fn [location]
                           [location (keep #(next-node location % chart nodes)
                                           (neighbors location chart))]))
                    nodes)]
    {:start start, :end end, :nodes nodes, :edges edges}))

(defn longest-path
  ([{:as analysis, :keys [start]}] (longest-path analysis start #{}))
  ([{:as analysis, :keys [end edges]} location seen]
   (if (= location end)
     0
     (let [seen (conj seen location)]
       (transduce (comp (remove (fn [[location _]] (seen location)))
                        (map (fn [[location distance]]
                               (+ distance
                                  (longest-path analysis location seen)))))
                  max
                  0
                  (edges location))))))

(defn answer-part-1 [chart]
  (longest-path (analyze-chart chart next-node)))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 94]
  [:puzzle 2414])


;;;; Part 2

(defn next-node2 [from to chart nodes]
  (loop [from from, to to, n 1]
    (when (walkable-tiles (get-in chart to))
      (if (nodes to)
        [to n]
        (recur to (first (remove #{from} (neighbors to chart))) (inc n))))))

(defn answer-part-2 [chart]
  (longest-path (analyze-chart chart next-node2)))

(core/set-answer-fn! 2 answer-part-2
  [:sample1 154]
  [:puzzle 6598])
