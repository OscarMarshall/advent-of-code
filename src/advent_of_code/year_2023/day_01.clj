(ns advent-of-code.year-2023.day-01
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(def input (core/get-input))

(defn parse-input [input] (string/split-lines input))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

(def numbers-regex #"^\d")
(def spelled-out->number
  {"one"   "1"
   "two"   "2"
   "three" "3"
   "four"  "4"
   "five"  "5"
   "six"   "6"
   "seven" "7"
   "eight" "8"
   "nine"  "9"})
(def spelled-out-numbers-regex
  (re-pattern (str "^(?:\\d|"
                   (string/join "|" (keys spelled-out->number))
                   ")")))

(defn find-numbers [s spelled-out]
  (->> s
       (iterate rest)
       (take-while seq)
       (keep (comp (partial re-find (if spelled-out
                                      spelled-out-numbers-regex
                                      numbers-regex))
                   (partial apply str)))
       (replace spelled-out->number)
       (map parse-long)))

(defn answer
  ([parsed-input] (answer parsed-input false))
  ([parsed-input spelled-out]
   (->> parsed-input
        (map (fn [s]
               (let [numbers (find-numbers s spelled-out)]
                 (+ (* (first numbers) 10) (last numbers)))))
        (apply +))))

(defn answer-part-1 [parsed-input] (answer parsed-input))


(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 54940))


;;; Part 2
;;; ============================================================================

(defn answer-part-2 [parsed-input] (answer parsed-input true))


(def part-2-answer (answer-part-2 parsed-input))

(assert (not= part-2-answer 54194))
(assert (not= part-2-answer 54618))
(assert (= part-2-answer 54208))
