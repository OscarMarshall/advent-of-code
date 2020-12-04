(ns advent-of-code-2020.day-03
  (:require [clojure.java.io :as io]))


;;; Part 1

(defn parse-input [file-name]
  (vec (line-seq (io/reader file-name))))

(def part-1-input (parse-input "src/advent_of_code_2020/day_03_part_1_input.txt"))

(defn check-slope [input right down]
  (let [width (count (first input))
        posns (map vector (iterate (partial + down) 0) (map #(mod % width) (iterate (partial + right) 0)))]
    (count (into [] (comp (map (partial get-in input)) (take-while some?) (filter #{\#})) posns))))

(defn answer-part-1 [input] (check-slope input 3 1))

(def part-1-answer (answer-part-1 part-1-input))

(comment
  part-1-answer
  ;; => 270
  )


;;; Part 2

(def part-2-input part-1-input)

(defn answer-part-2 [input]
  (apply * (map (partial apply check-slope input) [[1 1] [3 1] [5 1] [7 1] [1 2]])))

(def part-2-answer (answer-part-2 part-2-input))

(comment
  part-2-answer
  ;; => 2122848000
  )
