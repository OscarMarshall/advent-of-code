(ns advent-of-code.year-2021.day-06
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(def input (core/get-input))

(defn parse-input [input]
  (map #(Long/parseLong %) (string/split (string/trim input) #",")))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

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

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 360610))


;;; Part 2
;;; ============================================================================

(defn answer-part-2 [parsed-input]
  (number-of-lanternfish (frequencies parsed-input) 256))

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer 1631629590423))
