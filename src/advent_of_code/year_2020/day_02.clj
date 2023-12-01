(ns advent-of-code.year-2020.day-02
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(def input (core/get-input))

(defn parse-input [input]
  (into []
        (comp (map (partial re-matches #"(\d+)-(\d+) (.): (.*)"))
              (map (fn [[_ a b character password]]
                     [(Long/parseLong a) (Long/parseLong b) (first character)
                      password])))
        (string/split-lines input)))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

(defn answer-part-1 [parsed-input]
  (count (filter (fn [[minimum maximum character password]]
                   (<= minimum (count (filter #{character} password)) maximum))
                 parsed-input)))

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 465))


;;; Part 2
;;; ============================================================================

(defn answer-part-2 [parsed-input]
  (count (filter (fn [[posn1 posn2 character password]]
                   (let [posn-chars (into #{}
                                          (comp (map dec)
                                                (map (partial nth password)))
                                          [posn1 posn2])]
                     (and (posn-chars character) (= (count posn-chars) 2))))
                 parsed-input)))

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer 294))
