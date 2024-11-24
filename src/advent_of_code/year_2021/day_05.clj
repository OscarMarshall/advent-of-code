(ns advent-of-code.year-2021.day-05
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2021 5)


;;;; Parse

(defn parse-input [input]
  (map (fn [line]
         (->> line
              (re-matches #"(\d+),(\d+) -> (\d+),(\d+)")
              rest
              (map parse-long)
              (partition 2)
              vec))
       (string/split-lines input)))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn line->points [[[x1 y1] [x2 y2]]]
  (into #{[x2 y2]}
        (cond
          (= x1 x2) (map (fn [y] [x1 y])
                         (range y1 y2 (if (pos? (- y2 y1)) 1 -1)))
          (= y1 y2) (map (fn [x] [x y1])
                         (range x1 x2 (if (pos? (- x2 x1)) 1 -1)))
          :else     (map vector
                         (range x1 x2 (if (pos? (- x2 x1)) 1 -1))
                         (range y1 y2 (if (pos? (- y2 y1)) 1 -1))))))

(defn number-of-overlapping-points [lines]
  (->> lines
       (mapcat line->points)
       frequencies
       (filter (comp (partial < 1) val))
       count))

(defn answer-part-1 [parsed-input]
  (->> parsed-input
       (filter (comp (partial some true?) (partial apply map =)))
       number-of-overlapping-points))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 4728])


;;;; Part 2

(defn answer-part-2 [parsed-input] (number-of-overlapping-points parsed-input))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 17717])
