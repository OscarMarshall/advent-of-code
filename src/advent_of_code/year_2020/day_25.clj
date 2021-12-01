(ns advent-of-code.year-2020.day-25
  (:require [advent-of-code-2020.core :as core]
            [clojure.string :as string]))

(def input (core/get-input))

(defn parse-input [input] (map #(Long/parseLong %) (string/split-lines input)))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

(defn step [x subject-number] (rem (* x subject-number) 20201227))

(defn answer-part-1 [parsed-input]
  (let [[card-public-key door-public-key]
        parsed-input

        card-loops
        (count (take-while (complement #{card-public-key})
                           (iterate #(step % 7) 1)))]
    (nth (iterate #(step % door-public-key) 1) card-loops)))

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 15467093))
