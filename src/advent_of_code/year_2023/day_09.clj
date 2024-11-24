(ns advent-of-code.year-2023.day-09
  (:require [advent-of-code.core :as core]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)

(core/set-date! 2023 9)


;;;; Parse

(defn parse-input [input]
  (map #(mapv parse-long (str/split % #" ")) (str/split-lines input)))

(core/set-parse-fn! parse-input)

;;;; Part 1

(defn extrapolate [xs]
  (if (every? zero? xs)
    0
    (+ (peek xs) (extrapolate (mapv (fn [[x y]] (- y x)) (partition 2 1 xs))))))

(defn answer-part-1 [xs]
  (transduce (map extrapolate) + xs))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 114]
  [:puzzle 1898776583])


;;;; Part 2

(defn answer-part-2 [xs]
  (answer-part-1 (map #(vec (reverse %)) xs)))

(core/set-answer-fn! 2 answer-part-2
  [:sample1 2]
  [:puzzle 1100])
