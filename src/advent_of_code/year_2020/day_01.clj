(ns advent-of-code.year-2020.day-01
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2020 1)


;;;; Parse

(defn parse-input [input] (map parse-long (string/split-lines input)))

(core/set-parse-fn! parse-input)


;;;; Part 1

(def target 2020)

(defn find-2-expenses [target longs]
  (let [asc (sort longs), desc (reverse asc)]
    (loop [[a :as asc] asc, [z :as desc] desc]
      (when (and (some? a) (some? z))
        (let [sum (+ a z)]
          (cond
            (= sum target) [a z]
            (< sum target) (recur (rest asc) desc)
            (> sum target) (recur asc (rest desc))))))))

(defn answer-part-1 [parsed-input]
  (apply * (find-2-expenses target parsed-input)))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 299299])


;;;; Part 2

(defn answer-part-2 [parsed-input]
  (some (fn [[a & more]]
          (some->> more (find-2-expenses (- target a)) (apply * a)))
        (iterate next parsed-input)))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 287730716])
