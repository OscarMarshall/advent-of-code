(ns advent-of-code.day-00
  (:require [advent-of-code.core :as core]))

(set! *warn-on-reflection* true)

(core/set-date! 'year 'day)


;;;; Parse

(defn parse-input [input] input)

#_(core/set-parse-fn! parse-input)


;;;; Part 1

(defn answer-part-1 [x]
  x)

#_(core/set-answer-fn! 1 answer-part-1
    [:puzzle #_(get-in core/state [:outputs :puzzle])]
    #_(user/add-sample! ""))


;;;; Part 2

(defn answer-part-2 [x]
  x)

#_(core/set-answer-fn! 2 answer-part-2
    [:puzzle #_(get-in core/state [:outputs :puzzle])]
    #_(user/add-sample! ""))
