(ns advent-of-code.year-2024.day-04
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]
            [advent-of-code.utils :as utils]))

(set! *warn-on-reflection* true)

(core/set-date! 2024 4)


;;;; Parse

(defn parse-input [input] (utils/parse-2d-map input identity))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn answer-part-1 [{:keys [pois indices]}]
  (count (for [[x y] (indices \X)
               dx    [-1 0 1]
               dy    [-1 0 1]
               :when (and (not= dy dx 0)
                          (->> [x y]
                               (iterate (partial mapv + [dx dy]))
                               rest
                               (map pois)
                               (take 3)
                               (= [\M \A \S])))]
           true)))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 18]
  [:puzzle 2344])


;;;; Part 2

(defn answer-part-2 [{:keys [pois indices]}]
  (count (filter (fn [[x y]]
                   (every? (fn [vectors]
                             (= (into #{}
                                      (map (comp pois (partial mapv + [x y])))
                                     vectors)
                                #{\M \S}))
                           [[[-1 -1] [1 1]] [[-1 1] [1 -1]]]))
                 (indices \A))))

(core/set-answer-fn! 2 answer-part-2
  [:sample1 9]
  [:puzzle 1815])
