(ns advent-of-code.year-2022.day-03
  (:require [advent-of-code.core :as core]
            [clojure.set :as set]
            [clojure.string :as string]))

(def input (core/get-input))

(defn parse-input [input]
  (map (fn [s] (map set (split-at (/ (count s) 2) s)))
       (string/split-lines input)))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

(def char->priority
  (zipmap (sequence (map char)
                    (concat (range (int \a) (inc (int \z)))
                            (range (int \A) (inc (int \Z)))))
          (rest (range))))

(defn answer-part-1 [parsed-input]
  (transduce (map (comp char->priority first (partial apply set/intersection)))
             +
             parsed-input))

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 8053))


;;; Part 2
;;; ============================================================================

(defn answer-part-2 [parsed-input]
  (answer-part-1 (partition 3 (map (partial apply set/union) parsed-input))))

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer 2425))
