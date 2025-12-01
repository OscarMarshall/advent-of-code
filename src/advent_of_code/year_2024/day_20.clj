(ns advent-of-code.year-2024.day-20
  (:require [advent-of-code.core :as core]
            [advent-of-code.utils :as utils]
            [clojure.core.reducers :as r]))

(set! *warn-on-reflection* true)

(core/set-date! 2024 20)


;;;; Parse

(defn parse-input [input]
  (utils/parse-2d-map input {\# :wall, \. :track, \S :start, \E :end}))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn neighbors [[x y]]
  [[(inc x) y] [x (inc y)] [(dec x) y] [x (dec y)]])

(defn calculate-distances [pois width height coordinates]
  (let [grid (vec (repeat height (vec (repeat width nil))))]
    (reduce (fn [grid [coordinates distance]]
              (assoc-in grid coordinates distance))
            grid
            (utils/djkstra-walk (fn [coordinates]
                                  (eduction (remove (comp #{:wall} pois))
                                            (neighbors coordinates)))
                                coordinates))))

(defn cheats-from-count [cheat-time distance-grid [x y :as coordinates]]
  (let [current-distance (get-in distance-grid coordinates)]
    (transduce (comp (mapcat (fn [dx]
                               (let [remaining-distance (- cheat-time (abs dx))]
                                 (map (fn [dy] [dx dy])
                                      (range (- remaining-distance)
                                             (inc remaining-distance))))))
                     (keep (fn [[dx dy]]
                             (let [new-distance (-> distance-grid
                                                    (nth (+ x dx) nil)
                                                    (nth (+ y dy) nil))]
                               (when (and new-distance
                                          (>= (- current-distance
                                                 (+ new-distance
                                                    (abs dx)
                                                    (abs dy)))
                                              100))
                                 1)))))
               +
               (range (- cheat-time) (inc cheat-time)))))

(defn cheats-count [{:keys [pois indices width height]} cheat-time]
  (let [{:keys [track start end]} indices
        track                     (into track start)
        distance-grid             (calculate-distances pois
                                                       width
                                                       height
                                                       (first end))]
    (r/fold +
            (map (partial cheats-from-count cheat-time distance-grid) track))))

(defn answer-part-1 [map-info]
  (cheats-count map-info 2))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle [< 1476] 1463])


;;;; Part 2

(defn answer-part-2 [map-info]
  (cheats-count map-info 20))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle [> 985154] 985332])
