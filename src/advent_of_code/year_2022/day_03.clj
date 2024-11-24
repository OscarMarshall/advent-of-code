(ns advent-of-code.year-2022.day-03
  (:require [advent-of-code.core :as core]
            [clojure.set :as set]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2022 3)


;;;; Parse

(defn parse-input [input]
  (map (fn [s] (map set (split-at (/ (count s) 2) s)))
       (string/split-lines input)))

(core/set-parse-fn! parse-input)


;;;; Part 1

(def char->priority
  (zipmap (sequence (map char)
                    (concat (range (int \a) (inc (int \z)))
                            (range (int \A) (inc (int \Z)))))
          (rest (range))))

(defn answer-part-1 [parsed-input]
  (transduce (map (comp char->priority first (partial apply set/intersection)))
             +
             parsed-input))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 8053])


;;;; Part 2

(defn answer-part-2 [parsed-input]
  (answer-part-1 (partition 3 (map (partial apply set/union) parsed-input))))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 2425])
