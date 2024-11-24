(ns advent-of-code.year-2022.day-01
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2022 1)


;;;; Parse

(defn parse-input [input]
  (sequence (comp (partition-by empty?)
                  (remove #{[""]})
                  (map (partial map parse-long)))
            (string/split-lines input)))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn sorted-calorie-totals [list-of-calories]
  (sort (comp - compare) (map (partial apply +) list-of-calories)))

(defn answer-part-1 [parsed-input]
  (first (sorted-calorie-totals parsed-input)))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 68775])


;;;; Part 2

(defn answer-part-2 [parsed-input]
  (apply + (take 3 (sorted-calorie-totals parsed-input))))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 202585])
