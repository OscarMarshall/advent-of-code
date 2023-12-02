(ns advent-of-code.year-2023.day-01
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(def input (core/get-input))

(defn parse-input [input] (string/split-lines input))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

(def digit->long (into {} (map (fn [n] [(str n) n])) (range 1 10)))
(def digit-regex #"\d")

(defn first-occurence [digits line]
  (re-find (re-pattern (string/join "|" digits)) line))

(defn last-occurence [digits line]
  (second (re-find (re-pattern (str ".*(" (string/join "|" digits) ")")) line)))

(defn calibration-value [line digit->long]
  (let [digits (keys digit->long)]
    (parse-long (str (digit->long (first-occurence digits line))
                     (digit->long (last-occurence digits line))))))

(defn answer [parsed-input digit->long]
  (apply + (map #(calibration-value % digit->long) parsed-input)))

(defn answer-part-1 [parsed-input] (answer parsed-input digit->long))

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 54940))


;;; Part 2
;;; ============================================================================

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

(defn answer-part-2 [parsed-input]
  (answer parsed-input (merge digit->long spelled-out-digit->long)))

(def part-2-answer (answer-part-2 parsed-input))

(assert (not= part-2-answer 54194))
(assert (not= part-2-answer 54618))
(assert (= part-2-answer 54208))
