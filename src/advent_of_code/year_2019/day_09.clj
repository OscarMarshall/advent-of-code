(ns advent-of-code.year-2019.day-09
  (:require [advent-of-code.core :as core]
            [advent-of-code.year-2019.intcode-computer :as intcode-computer]))

(set! *warn-on-reflection* true)

(core/set-date! 2019 9)


;;;; Parse

(core/set-parse-fn! intcode-computer/parse-program)


;;;; Part 1

(defn answer-part-1 [program]
  (last (:outputs (intcode-computer/start program 1))))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 2399197539])


;;;; Part 2

(defn answer-part-2 [program]
  (last (:outputs (intcode-computer/start program 2))))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 35106])
