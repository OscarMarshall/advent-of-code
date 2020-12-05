(ns advent-of-code-2020.day-02
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))


;;; Part 1

(defn parse-input [file-name]
  (into []
        (comp (map (partial re-matches #"(\d+)-(\d+) (.): (.*)"))
              (map (fn [[_ a b character password]]
                     [(edn/read-string a) (edn/read-string b) (first character) password])))
        (line-seq (io/reader file-name))))

(def input (parse-input "src/advent_of_code_2020/day_02_input.txt"))

(defn answer-part-1 [input]
  (count (filter (fn [[minimum maximum character password]] (<= minimum (count (filter #{character} password)) maximum))
                 input)))

(def part-1-answer (answer-part-1 input))

(comment
  part-1-answer
  ;; => 465
  )

;;; Part 2

(defn answer-part-2 [input]
  (count (filter (fn [[posn1 posn2 character password]]
                   (let [posn-chars (into #{}
                                          (comp (map dec) (map (partial nth password)))
                                          [posn1 posn2])]
                     (and (posn-chars character) (= (count posn-chars) 2))))
                 input)))

(def part-2-answer (answer-part-2 input))

(comment
  part-2-answer
  ;; => 294
  )
