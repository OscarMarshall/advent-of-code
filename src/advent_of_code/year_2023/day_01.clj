(ns advent-of-code.year-2023.day-01
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(def input (core/get-input))

(defn parse-input [input] (string/split-lines input))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

(def number-regexes [#"\d" #"\d"])
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
(def spelled-out-number-regexes
  (let [inner-regex-string (string/join "|" (keys spelled-out->number))]
    (mapv (fn [f] (re-pattern (str "\\d|" (f inner-regex-string))))
          [identity string/reverse])))

(defn calibration-value [line spelled-out]
  (let [[forward-regex backward-regex] (if spelled-out
                                         spelled-out-number-regexes
                                         number-regexes)]
    (->> [(re-find forward-regex line)
          (string/reverse (re-find backward-regex (string/reverse line)))]
         (replace spelled-out->number)
         string/join
         parse-long)))

(defn answer
  ([parsed-input] (answer parsed-input false))
  ([parsed-input spelled-out]
   (->> parsed-input
        (map #(calibration-value % spelled-out))
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
