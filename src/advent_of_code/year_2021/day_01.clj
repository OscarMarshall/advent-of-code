(ns advent-of-code.year-2021.day-01
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2021 1)


;;;; Parse

(defn parse-input [input] (map parse-long (string/split-lines input)))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn answer-part-1 [parsed-input]
  (count (filter (partial apply <) (partition 2 1 parsed-input))))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 1400])


;;;; Part 2

(defn answer-part-2 [parsed-input]
  (answer-part-1 (map (partial apply +) (partition 3 1 parsed-input))))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 1429])
