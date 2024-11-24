(ns advent-of-code.year-2021.day-06
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2021 06)


;;;; Parse

(defn parse-input [input]
  (map parse-long (string/split (string/trim input) #",")))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn simulate-day [{zeros 0, :as state, :or {zeros 0}}]
  (-> state
      (dissoc 0)
      (->> (into {} (map (juxt (comp dec key) val))))
      (update 6 (fnil + 0) zeros)
      (assoc 8 zeros)))

(defn number-of-lanternfish [state days]
  (apply + (vals (nth (iterate simulate-day state) days))))

(defn answer-part-1 [parsed-input]
  (number-of-lanternfish (frequencies parsed-input) 80))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 360610])


;;;; Part 2

(defn answer-part-2 [parsed-input]
  (number-of-lanternfish (frequencies parsed-input) 256))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 1631629590423])
