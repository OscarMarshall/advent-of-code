(ns advent-of-code.year-2023.day-01
  (:require [advent-of-code.core :as core]
            [clojure.string :as str]
            [medley.core :as medley]))

(def input (core/get-input *file*))

(defn parse-input [input] (str/split-lines input))

(def parsed-input (parse-input input))


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

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 54940))


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

(def part-2-answer (answer-part-2 parsed-input))

(assert (not= part-2-answer 54194))
(assert (not= part-2-answer 54618))
(assert (= part-2-answer 54208))
