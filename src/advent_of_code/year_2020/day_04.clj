(ns advent-of-code.year-2020.day-04
  (:require [advent-of-code.core :as core]
            [clojure.set :as set]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2020 4)


;;;; Parse

(defn parse-input [input]
  (map (fn [passport]
         (into {}
               (map #(string/split % #":"))
               (string/split passport #"[ \n]")))
       (string/split input #"\n\n")))

(core/set-parse-fn! parse-input)


;;;; Part 1

(def required-fields #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"})

(defn answer-part-1 [parsed-input]
  (count (filter #(set/superset? (into #{} (keys %)) required-fields)
                 parsed-input)))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 264])


;;;; Part 2

(defn year-validator [minimum maximum]
  #(when-let [year (re-matches #"\d{4}" %)]
     (<= minimum (parse-long year) maximum)))

(def validators
  {"byr" (year-validator 1920 2020)
   "iyr" (year-validator 2010 2020)
   "eyr" (year-validator 2020 2030)
   "hgt" #(when-let [[_ x units] (re-matches #"(\d+)(cm|in)" %)]
            (let [x (parse-long x)]
              (case units
                "cm" (<= 150 x 193)
                "in" (<= 59 x 76))))
   "hcl" (partial re-matches #"#[0-9a-f]{6}")
   "ecl" #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}
   "pid" (partial re-matches #"[0-9]{9}")})

(defn answer-part-2 [parsed-input]
  (count (filter (fn [passport]
                   (every? (fn [[k validate]]
                             (some-> k passport validate))
                           validators))
                 parsed-input)))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 224])
