(ns advent-of-code.year-2020.day-10
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2020 10)


;;;; Parse

(defn parse-input [input]
  (map parse-long (string/split-lines input)))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn answer-part-1 [parsed-input]
  (let [joltage-adapters   (sort parsed-input)
        joltage-adapters   (concat (cons 0 joltage-adapters)
                                   [(+ (last joltage-adapters) 3)])
        {ones 1, threes 3} (frequencies (map #(- %2 %1)
                                             joltage-adapters
                                             (rest joltage-adapters)))]
    (* ones threes)))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 2040])


;;;; Part 2

(def count-arrangements
  (memoize
   (fn [[a b :as joltage-adapters] prev]
     (if (nil? a)
       1
       (let [joltage-adapters (rest joltage-adapters)]
         (cond-> (count-arrangements joltage-adapters a)
           (<= (- (or b (+ a 3)) prev) 3)
           (+ (count-arrangements joltage-adapters prev))))))))

(defn answer-part-2 [parsed-input]
  (count-arrangements (sort parsed-input) 0))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 28346956187648])
