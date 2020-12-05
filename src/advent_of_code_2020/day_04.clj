(ns advent-of-code-2020.day-04
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [clojure.edn :as edn]))


;;; Part 1

(defn parse-input [file-name]
  (map (fn [passport]
         (-> passport
             (string/split #"[ \n]")
             (->> (map #(string/split % #":"))
                  (into {}))))
       (string/split (slurp file-name) #"\n\n")))

(def input (parse-input "src/advent_of_code_2020/day_04_input.txt"))

(defn answer-part-1 [input]
  (count (filter #(set/superset? (into #{} (keys %)) #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"}) input)))

(def part-1-answer (answer-part-1 input))

(comment
  part-1-answer
  ;; => 264
  )


;;; Part 2

(defn year-validator [minimum maximum]
  #(when-let [year (re-matches #"\d{4}" %)]
     (<= minimum (edn/read-string year) maximum)))

(def validators
  {"byr" (year-validator 1920 2020)
   "iyr" (year-validator 2010 2020)
   "eyr" (year-validator 2020 2030)
   "hgt" #(when-let [[_ x units] (re-matches #"(\d+)(cm|in)" %)]
            (let [x (edn/read-string x)]
              (case units
                "cm" (<= 150 x 193)
                "in" (<= 59 x 76))))
   "hcl" (partial re-matches #"#[0-9a-f]{6}")
   "ecl" #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}
   "pid" (partial re-matches #"[0-9]{9}")})

(defn answer-part-2 [input]
  (count (filter (fn [passport] (every? (fn [[k validate]] (some-> k passport validate)) validators)) input)))

(def part-2-answer (answer-part-2 input))

(comment
  part-2-answer
  ;; => 224
  )
