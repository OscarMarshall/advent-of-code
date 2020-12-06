(ns advent-of-code-2020.day-06
  (:require [advent-of-code-2020.core :as core]
            [clojure.set :as set]
            [clojure.string :as string]))

;;; Part 1
;;; ============================================================================

(def input (core/get-input))

(defn parse-input [file-name]
  (map (comp (partial map set) string/split-lines)
       (string/split (slurp file-name) #"\n\n")))

(def parsed-input (parse-input "src/advent_of_code_2020/day_06_input.txt"))

(defn reduce-groups [reduce-members parsed-input]
  (transduce (map (comp count reduce-members)) + parsed-input))

(defn answer-part-1 [parsed-input]
  (reduce-groups (partial apply set/union) parsed-input))

(def part-1-answer (answer-part-1 parsed-input))

(comment
  part-1-answer
  ;; => 6596
  )

;;; Part 2
;;; ============================================================================

(defn answer-part-2 [parsed-input]
  (reduce-groups (partial apply set/intersection) parsed-input))

(def part-2-answer (answer-part-2 parsed-input))

(comment
  part-2-answer
  ;; => 3219
  )
