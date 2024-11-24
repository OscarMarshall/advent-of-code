(ns advent-of-code.year-2020.day-25
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2020 25)


;;;; Parse

(defn parse-input [input] (map parse-long (string/split-lines input)))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn step [x subject-number] (rem (* x subject-number) 20201227))

(defn answer-part-1 [parsed-input]
  (let [[card-public-key door-public-key]
        parsed-input

        card-loops
        (count (take-while (complement #{card-public-key})
                           (iterate #(step % 7) 1)))]
    (nth (iterate #(step % door-public-key) 1) card-loops)))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 15467093])
