(ns advent-of-code-2020.day-05
  (:require [clojure.java.io :as io]))


;;; Part 1

(defn parse-input [file-name]
  (line-seq (io/reader file-name)))

(def input (parse-input "src/advent_of_code_2020/day_05_input.txt"))

(defn input->seat-ids [input]
  (into []
        (comp (map (partial replace {\F 0 \B 1 \L 0 \R 1})) (map (partial apply str)) (map #(Long/parseLong % 2)))
        input))

(defn answer-part-1 [input]
  (apply max (input->seat-ids input)))

(def part-1-answer (answer-part-1 input))

(comment
  part-1-answer
  ;; => 944
  )


;;; Part 2

(defn answer-part-2 [input]
  (let [seat-ids (vec (sort (input->seat-ids input)))]
    (first (reduce disj (set (range (first seat-ids) (inc (last seat-ids)))) seat-ids))))

(def part-2-answer (answer-part-2 input))

(comment
  part-2-answer
  ;; => 554
  )
