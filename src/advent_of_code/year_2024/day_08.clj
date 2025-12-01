(ns advent-of-code.year-2024.day-08
  (:require [advent-of-code.core :as core]
            [advent-of-code.utils :as utils]
            [clojure.math.combinatorics :as combo]))

(set! *warn-on-reflection* true)

(core/set-date! 2024 8)


;;;; Parse

(defn parse-input [input]
  (utils/parse-2d-map input (fn [c] (when (not= c \.) c))))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn in-bounds? [width height [x y]] (and (< -1 x width) (< -1 y height)))

(defn antinodes [height width antenna1 antenna2]
  (let [difference (mapv - antenna2 antenna1)]
    (eduction (filter (partial in-bounds? width height))
              [(mapv - antenna1 difference) (mapv + antenna2 difference)])))

(defn answer-part-1 [{:keys [indices height width]}]
  (->> indices
       vals
       (into #{} (mapcat (fn [antennas]
                           (eduction (mapcat (fn [[antenna1 antenna2]]
                                               (antinodes width
                                                          height
                                                          antenna1
                                                          antenna2)))
                                     (combo/combinations antennas 2)))))
       count))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 14]
  [:puzzle 291])


;;;; Part 2

(defn resonant-antinodes [height width antenna1 antenna2]
  (let [difference (mapv - antenna2 antenna1)]
    (eduction (mapcat (fn [op]
                        (take-while (partial in-bounds? height width)
                                    (iterate #(mapv op % difference)
                                             antenna1))))
              [+ -])))

(defn answer-part-2 [{:keys [indices height width]}]
  (->> indices
       vals
       (into #{} (mapcat (fn [antennas]
                           (eduction (mapcat (fn [[antenna1 antenna2]]
                                               (resonant-antinodes height
                                                                   width
                                                                   antenna1
                                                                   antenna2)))
                                     (combo/combinations antennas 2)))))
       count))

(core/set-answer-fn! 2 answer-part-2
  [:sample1 34]
  [:puzzle 1015])
