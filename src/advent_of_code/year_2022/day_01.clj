(ns advent-of-code.year-2022.day-01
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(def input (core/get-input *file*))

(defn parse-input [input]
  (sequence (comp (partition-by empty?)
                  (remove #{[""]})
                  (map (partial map #(Integer/parseInt %))))
            (string/split-lines input)))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

(defn sorted-calorie-totals [list-of-calories]
  (sort (comp - compare) (map (partial apply +) list-of-calories)))

(defn answer-part-1 [parsed-input]
  (first (sorted-calorie-totals parsed-input)))

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 68775))


;;; Part 2
;;; ============================================================================

(defn answer-part-2 [parsed-input]
  (apply + (take 3 (sorted-calorie-totals parsed-input))))

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer 202585))
