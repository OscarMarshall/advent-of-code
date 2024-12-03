(ns advent-of-code.year-2024.day-02
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]
            [medley.core :as medley]))

(set! *warn-on-reflection* true)

(core/set-date! 2024 2)


;;;; Parse

(defn parse-input [input]
  (map (fn [line] (map parse-long (string/split line #" ")))
       (string/split-lines input)))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn safe-report? [report]
  (let [differences (map - report (rest report))]
    (or (every? #{1 2 3} differences) (every? #{-1 -2 -3} differences))))

(defn answer-part-1 [reports]
  (count (filter safe-report? reports)))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 2]
  [:puzzle 639])


;;;; Part 2

(defn problem-dampened-safe-report? [report]
  (some safe-report?
        (cons report
              (map #(medley/remove-nth % report) (range (count report))))))

(defn answer-part-2 [reports]
  (count (filter problem-dampened-safe-report? reports)))

(core/set-answer-fn! 2 answer-part-2
  [:sample1 4]
  [:puzzle [> 672] 674])
