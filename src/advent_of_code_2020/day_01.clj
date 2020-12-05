(ns advent-of-code-2020.day-01
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))


;;; Part 1

(defn parse-input [file-name]
  (map edn/read-string (line-seq (io/reader file-name))))

(def input (parse-input "src/advent_of_code_2020/day_01_input.txt"))

(defn find-2-expenses [target longs]
  (let [asc (sort longs), desc (reverse asc)]
    (loop [[a :as asc] asc, [z :as desc] desc]
      (when (and (some? a) (some? z))
        (let [sum (+ a z)]
          (cond
            (= sum target) [a z]
            (< sum target) (recur (rest asc) desc)
            (> sum target) (recur asc (rest desc))))))))

(defn answer-part-1 [input]
  (apply * (find-2-expenses 2020 input)))

(def part-1-answer (answer-part-1 input))

(comment
  part-1-answer
  ;; => 299299
  )


;;; Part 2

(defn answer-part-2 [input]
  (some (fn [[a & input]] (some->> input (find-2-expenses (- 2020 a)) (apply * a))) (iterate next input)))

(def part-2-answer (answer-part-2 input))

(comment
  part-2-answer
  ;; => 287730716
  )
