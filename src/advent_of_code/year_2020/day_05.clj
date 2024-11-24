(ns advent-of-code.year-2020.day-05
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2020 5)


;;;; Parse

(defn parse-input [input]
  (string/split-lines input))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn seat-ids [parsed-input]
  (into []
        (comp (map (partial replace {\F 0 \B 1 \L 0 \R 1}))
              (map (partial apply str))
              (map #(Long/parseLong % 2)))
        parsed-input))

(defn answer-part-1 [parsed-input]
  (apply max (seat-ids parsed-input)))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 944])


;;;; Part 2

(defn all-seats [seat-ids]
  (let [seat-ids (sort seat-ids)]
    (set (range (first seat-ids) (inc (last seat-ids))))))

(defn answer-part-2 [parsed-input]
  (let [seat-ids (vec (sort (seat-ids parsed-input)))]
    (first (reduce disj (all-seats seat-ids) seat-ids))))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 554])
