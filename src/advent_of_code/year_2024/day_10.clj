(ns advent-of-code.year-2024.day-10
  (:require [advent-of-code.core :as core]
            [advent-of-code.utils :as utils]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2024 10)


;;;; Parse

(defn parse-input [input] (utils/parse-2d-map input (comp parse-long str)))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn neighbors [[x y]]
  [[(inc x) y] [x (inc y)] [(dec x) y] [x (dec y)]])

(defn answer-part-1 [{:keys [pois indices]}]
  (transduce (map (fn [coordinates]
                    (->> coordinates
                         (utils/djkstra-walk
                          (fn [coordinates]
                            (let [height (pois coordinates)]
                              (filter (comp #{(inc height)} pois)
                                      (neighbors coordinates)))))
                         (filter (comp #{9} pois first))
                         count)))
             +
             (indices 0)))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 36]
  [:puzzle 717])


;;;; Part 2

(defn distinct-hiking-trails [pois coordinates]
  (->> coordinates
       (utils/djkstra-walk
        (fn [coordinates]
          (let [height (pois coordinates)]
            (filter (comp #{(inc height)} pois)
                    (neighbors coordinates)))))
       (filter (comp #{9} pois first))
       (mapcat last)
       count))

(defn answer-part-2 [{:keys [pois indices]}]
  (transduce (map (partial distinct-hiking-trails pois)) + (indices 0)))

(core/set-answer-fn! 2 answer-part-2
  [:sample1 81]
  [:puzzle 1686])
