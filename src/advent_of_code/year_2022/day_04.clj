(ns advent-of-code.year-2022.day-04
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(def input (core/get-input *file*))

(defn parse-input [input]
  (map (fn [s]
         (mapv (fn [s]
                 (mapv (fn [s] (Integer/parseInt s)) (string/split s #"-")))
               (string/split s #",")))
       (string/split-lines input)))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

(defn answer-part-1 [parsed-input]
  (count (filter (fn [[[a1 b1] [a2 b2]]] (or (<= a1 a2 b2 b1) (<= a2 a1 b1 b2)))
                 parsed-input)))

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 573))


;;; Part 2
;;; ============================================================================

(defn answer-part-2 [parsed-input]
  (count (filter (fn [[[a1 b1] [a2 b2]]]
                   (or (<= a1 a2 b1) (<= a1 b2 b1) (<= a2 a1 b2) (<= a2 b1 b2)))
                 parsed-input)))

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer 867))
