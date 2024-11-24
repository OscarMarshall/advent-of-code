(ns advent-of-code.year-2020.day-02
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2020 2)


;;;; Parse

(defn parse-input [input]
  (into []
        (comp (map (partial re-matches #"(\d+)-(\d+) (.): (.*)"))
              (map (fn [[_ a b character password]]
                     [(parse-long a) (parse-long b) (first character)
                      password])))
        (string/split-lines input)))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn answer-part-1 [parsed-input]
  (count (filter (fn [[minimum maximum character password]]
                   (<= minimum (count (filter #{character} password)) maximum))
                 parsed-input)))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 465])


;;;; Part 2

(defn answer-part-2 [parsed-input]
  (count (filter (fn [[posn1 posn2 character password]]
                   (let [posn-chars (into #{}
                                          (comp (map dec)
                                                (map (partial nth password)))
                                          [posn1 posn2])]
                     (and (posn-chars character) (= (count posn-chars) 2))))
                 parsed-input)))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 294])
