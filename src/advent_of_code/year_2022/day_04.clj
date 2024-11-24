(ns advent-of-code.year-2022.day-04
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2022 4)


;;;; Parse

(defn parse-input [input]
  (map (fn [s]
         (mapv (fn [s] (mapv parse-long (string/split s #"-")))
               (string/split s #",")))
       (string/split-lines input)))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn answer-part-1 [parsed-input]
  (count (filter (fn [[[a1 b1] [a2 b2]]] (or (<= a1 a2 b2 b1) (<= a2 a1 b1 b2)))
                 parsed-input)))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 573])


;;;; Part 2

(defn answer-part-2 [parsed-input]
  (count (filter (fn [[[a1 b1] [a2 b2]]]
                   (or (<= a1 a2 b1) (<= a1 b2 b1) (<= a2 a1 b2) (<= a2 b1 b2)))
                 parsed-input)))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 867])
