(ns advent-of-code.year-2020.day-03
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(def input (core/get-input *file*))

(defn parse-input [input]
  (vec (string/split-lines input)))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

(defn check-slope [parsed-input right down]
  (let [width (count (first parsed-input))
        posns (map vector
                   (iterate #(+ % down) 0)
                   (map #(mod % width) (iterate #(+ % right) 0)))]
    (count (into []
                 (comp (map (partial get-in parsed-input))
                       (take-while some?)
                       (filter #{\#}))
                 posns))))

(defn answer-part-1 [parsed-input] (check-slope parsed-input 3 1))

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 270))


;;; Part 2
;;; ============================================================================

(def slopes [[1 1] [3 1] [5 1] [7 1] [1 2]])

(defn answer-part-2 [parsed-input]
  (apply * (map (partial apply check-slope parsed-input) slopes)))

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer 2122848000))
