(ns advent-of-code.year-2023.day-01
  (:require [advent-of-code.core :as core]
            [clojure.string :as str]
            [medley.core :as medley]))

(set! *warn-on-reflection* true)

(core/set-date! 2023 1)


;;;; Parse

(defn parse-input [input] (str/split-lines input))

(core/set-parse-fn! parse-input)


;;;; Part 1

(def digit->long (medley/index-by str (range 1 10)))
(def digit-regex #"\d")

(defn first-occurence [digits line]
  (re-find (re-pattern (str/join "|" digits)) line))

(defn last-occurence [digits line]
  (second (re-find (re-pattern (str ".*(" (str/join "|" digits) ")")) line)))

(defn calibration-value [line digit->long]
  (let [digits (keys digit->long)]
    (parse-long (str (digit->long (first-occurence digits line))
                     (digit->long (last-occurence digits line))))))

(defn answer [lines digit->long]
  (transduce (map #(calibration-value % digit->long)) + lines))

(defn answer-part-1 [lines] (answer lines digit->long))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 142]
  [:puzzle 54940])


;;;; Part 2

(def spelled-out-digit->long
  {"one"   1
   "two"   2
   "three" 3
   "four"  4
   "five"  5
   "six"   6
   "seven" 7
   "eight" 8
   "nine"  9})

(defn answer-part-2 [lines]
  (answer lines (merge digit->long spelled-out-digit->long)))

(core/set-answer-fn! 2 answer-part-2
  [:sample2 281]
  [:puzzle [> 54194] [< 54618] 54208])
