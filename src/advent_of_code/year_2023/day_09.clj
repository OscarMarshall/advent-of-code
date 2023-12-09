(ns advent-of-code.year-2023.day-09
  (:require [advent-of-code.core :as core]
            [clojure.string :as str]))

(println "# Day 9")

(set! *warn-on-reflection* true)

(defn parse-input [input]
  (map #(mapv parse-long (str/split % #" ")) (str/split-lines input)))

;;;; Part 1

(defn extrapolate [xs]
  (if (every? zero? xs)
    0
    (+ (peek xs)
       (extrapolate (mapv #(- (second %) (first %)) (partition 2 1 xs))))))

(defn answer-part-1 [xs]
  (transduce (map extrapolate) + xs))

(core/part 1
  parse-input answer-part-1 *file*
  [:sample1 114]
  [:input 1898776583])


;;;; Part 2

(defn answer-part-2 [x]
  x)

(core/part 2
  parse-input answer-part-2 *file*
  #_[:sample1]
  [:input])
