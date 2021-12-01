(ns advent-of-code.year-2021.day-01
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(def input (core/get-input))

(defn parse-input [input] (map #(Long/parseLong %) (string/split-lines input)))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

(defn answer-part-1 [parsed-input]
  (count (filter (fn [[x y]] (< x y)) (partition 2 1 parsed-input))))

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 1400))


;;; Part 2
;;; ============================================================================

(defn answer-part-2 [parsed-input]
  (answer-part-1 (map (partial apply +) (partition 3 1 parsed-input))))

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer 1429))
