(ns advent-of-code.day-00
  (:require [advent-of-code.core :as core]))

(println "HEADER")

(set! *warn-on-reflection* true)

(defn parse-input [input] input)

(comment
  (core/current-parsed-input :sample1)
  (core/current-parsed-input)
  )


;;;; Part 1

(defn answer-part-1 [x]
  x)

(core/part 1
  parse-input answer-part-1 *file*
  (comment (do (require 'user) (user/add-sample! "")))
  [:input #_(core/current-answer 1)])


;;;; Part 2

(defn answer-part-2 [x]
  x)

(core/part 2
  parse-input answer-part-2 *file*
  (comment (do (require 'user) (user/add-sample! "")))
  [:input #_(core/current-answer 2)])
