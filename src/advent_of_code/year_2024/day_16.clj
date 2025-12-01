(ns advent-of-code.year-2024.day-16
  (:require [advent-of-code.core :as core]
            [advent-of-code.utils :as utils]))

(set! *warn-on-reflection* true)

(core/set-date! 2024 16)


;;;; Parse

(defn parse-input [input]
  (let [{:keys [pois indices]} (utils/parse-2d-map input {\# :wall
                                                          \S :start
                                                          \E :end})]
    {:maze pois
     :start (first (:start indices))
     :end (first (:end indices))}))

(core/set-parse-fn! parse-input)


;;;; Part 1

(def direction->vector
  {:north [0 -1]
   :east  [1 0]
   :south [0 1]
   :west  [-1 0]})

(def turns
  {:north [:east :west]
   :east  [:south :north]
   :south [:west :east]
   :west  [:north :south]})

(defn fastest-paths [maze start end]
  (->> [start :east]
       (utils/weighted-djkstra-walk
        (fn [[position direction]]
          (keep (fn [[direction points]]
                  (let [position (mapv + position (direction->vector direction))]
                    (when (not= (maze position) :wall)
                      [[position direction] points])))
                (cons [direction 1]
                      (map (fn [direction] [direction 1001])
                           (turns direction))))))
       (some (fn [[[position] points paths]]
               (when (= position end) [points paths])))))

(defn answer-part-1 [{:keys [maze start end]}]
  (first (fastest-paths maze start end)))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 7036]
  [:sample2 11048]
  [:sample3 21148]
  [:sample4 4009]
  [:sample5 9029]
  [:sample6 4013]
  [:puzzle [< 90448] 90440])


;;;; Part 2

(defn answer-part-2 [{:keys [maze start end]}]
  (count (into #{}
               (comp cat (map first))
               (second (fastest-paths maze start end)))))

(core/set-answer-fn! 2 answer-part-2
  [:sample1 45]
  [:sample2 64]
  [:puzzle 479])
