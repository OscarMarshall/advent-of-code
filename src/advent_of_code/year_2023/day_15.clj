(ns advent-of-code.year-2023.day-15
  (:require [advent-of-code.core :as core]
            [clojure.string :as str]))

(println "# Day 15")

(set! *warn-on-reflection* true)

(defn parse-input [input] (str/split (str/replace input "\n" "") #","))

(comment
  (core/current-parsed-input :sample1)
  (core/current-parsed-input)
  )

;;;; Part 1

(defn hash-instruction [s]
  (transduce (map long)
             (completing (fn [current-value ascii-code]
                           (mod (* (+ current-value ascii-code) 17) 256)))
             0
             s))

(defn answer-part-1 [instructions]
  (transduce (map hash-instruction)
             +
             instructions))

(core/part 1
  parse-input answer-part-1 *file*
  [:sample1 1320]
  [:input [not= 517847] 517965])


;;;; Part 2

(defn answer-part-2 [x]
  x)

(core/part 2
  parse-input answer-part-2 *file*
  [:sample1 #_?]
  [:input #_(core/current-answer 2)])
