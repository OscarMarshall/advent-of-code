(ns advent-of-code.year-2022.day-02
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2022 2)


;;;; Parse

(defn parse-input [input]
  (mapv (fn [s] (mapv keyword (string/split s #" ")))
        (string/split-lines input)))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn answer-part-1 [parsed-input]
  (apply + (map (fn [[opponent me]]
                  (+ ({:X 1, :Y 2, :Z 3} me)
                     (case [opponent me]
                       ([:A :Z] [:B :X] [:C :Y]) 0 ; Loss
                       ([:A :X] [:B :Y] [:C :Z]) 3 ; Tie
                       ([:A :Y] [:B :Z] [:C :X]) 6))) ; Win
                parsed-input)))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 12156])


;;;; Part 2

(defn answer-part-2 [parsed-input]
  (apply + (map (fn [[opponent outcome]]
                  (+ (case [opponent outcome]
                       ([:B :X] [:A :Y] [:C :Z]) 1 ; Rock
                       ([:C :X] [:B :Y] [:A :Z]) 2 ; Paper
                       ([:A :X] [:C :Y] [:B :Z]) 3) ; Scissors
                     ({:X 0, :Y 3, :Z 6} outcome))) ; Win
                parsed-input)))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 10835])
