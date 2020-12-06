(ns advent-of-code-2020.day-06
  (:require [clojure.set :as set]
            [clojure.string :as string]))

;;; Part 1
;;; ============================================================================

(defn parse-input [file-name]
  (map (comp (partial map set) string/split-lines)
       (string/split (slurp file-name) #"\n\n")))

(def input (parse-input "src/advent_of_code_2020/day_06_input.txt"))

(defn answer-part-1 [input]
  (transduce (map (fn [group] (count (apply set/union group)))) + input))

(def part-1-answer (answer-part-1 input))

(comment
  part-1-answer
  ;; => 6596
  )

;;; Part 2
;;; ============================================================================

(defn answer-part-2 [input]
  (transduce (map (fn [group] (count (apply set/intersection group)))) + input))

(def part-2-answer (answer-part-2 input))

(comment
  part-2-answer
  ;; => 3219
  )
