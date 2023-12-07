(ns advent-of-code.day-00
  (:require [advent-of-code.core :as core]))

(println "# Day " 00)

(set! *warn-on-reflection* true)

(defn parse-input [input] input)

(def sample-input (core/get-input *file* :sample))
(def sample-parsed-input (parse-input sample-input))

(def input (core/get-input *file*))
(def parsed-input (parse-input input))


;;;; Part 1

(println "## Part 1")

(defn answer-part-1 [x]
  x)

(println "### Sample Answer")

(def part-1-sample-answer (time (answer-part-1 sample-parsed-input)))

(prn part-1-sample-answer)
(println)

(println "### Answer")

(def part-1-answer (time (answer-part-1 parsed-input)))

(prn part-1-answer)
(println)

(assert (= part-1-sample-answer))
(assert (= part-1-answer part-1-answer))


;;;; Part 2

(println "## Part 2")

(defn answer-part-2 [x]
  x)

(println "### Sample Answer")

(def part-2-sample-answer (time (answer-part-2 sample-parsed-input)))

(prn part-2-sample-answer)
(println)

(println "### Answer")

(def part-2-answer (time (answer-part-2 parsed-input)))

(prn part-2-answer)
(println)

(assert (= part-2-sample-answer))
(assert (= part-2-answer part-2-answer))
