(ns advent-of-code.year-2020.day-01
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(def input (core/get-input *file*))

(defn parse-input [input]
  (map #(Long/parseLong %) (string/split-lines input)))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

(def target 2020)

(defn find-2-expenses [target longs]
  (let [asc (sort longs), desc (reverse asc)]
    (loop [[a :as asc] asc, [z :as desc] desc]
      (when (and (some? a) (some? z))
        (let [sum (+ a z)]
          (cond
            (= sum target) [a z]
            (< sum target) (recur (rest asc) desc)
            (> sum target) (recur asc (rest desc))))))))

(defn answer-part-1 [parsed-input]
  (apply * (find-2-expenses target parsed-input)))

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 299299))


;;; Part 2
;;; ============================================================================

(defn answer-part-2 [parsed-input]
  (some (fn [[a & more]]
          (some->> more (find-2-expenses (- target a)) (apply * a)))
        (iterate next parsed-input)))

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer 287730716))
