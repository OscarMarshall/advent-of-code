(ns advent-of-code.year-2019.day-05
  (:require [advent-of-code.core :as core]
            [advent-of-code.year-2019.intcode-computer :as intcode-computer]))

(set! *warn-on-reflection* true)

(core/set-date! 2019 5)


;;;; Parse

(core/set-parse-fn! intcode-computer/parse-program)


;;;; Part 1

(defn answer-part-1 [program]
  (last (:outputs (intcode-computer/start program 1))))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 7839346])


;;;; Part 2

(defn answer-part-2 [program]
  (first (:outputs (intcode-computer/start program 5))))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 447803])
