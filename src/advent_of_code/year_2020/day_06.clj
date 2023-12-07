(ns advent-of-code.year-2020.day-06
  (:require [advent-of-code.core :as core]
            [clojure.set :as set]
            [clojure.string :as string]))

(def input (core/get-input *file*))

(defn parse-input [input]
  (map (comp (partial map set) string/split-lines)
       (string/split input #"\n\n")))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

(defn reduce-groups [reduce-members parsed-input]
  (transduce (map (comp count reduce-members)) + parsed-input))

(defn answer-part-1 [parsed-input]
  (reduce-groups (partial apply set/union) parsed-input))

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 6596))


;;; Part 2
;;; ============================================================================

(defn answer-part-2 [parsed-input]
  (reduce-groups (partial apply set/intersection) parsed-input))

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer 3219))
