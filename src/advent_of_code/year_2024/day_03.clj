(ns advent-of-code.year-2024.day-03
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2024 3)


;;;; Parse

(core/set-parse-fn! identity)


;;;; Part 1

(defn answer-part-1 [memory]
  (transduce (map (fn [[_ x y]] (* (parse-long x) (parse-long y))))
             +
             (re-seq #"mul\((\d+),(\d+)\)" memory)))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 161]
  [:puzzle 196826776])


;;;; Part 2

(defn answer-part-2 [memory]
  (answer-part-1 (string/replace memory #"don't\(\)(?:.|\n)*?(?:do\(\)|$)" "")))

(core/set-answer-fn! 2 answer-part-2
  [:sample2 48]
  [:puzzle [not= 137035868] 106780429])
