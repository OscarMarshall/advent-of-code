(ns advent-of-code.year-2025.day-01
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2025 1)


;;;; Parse

(defn parse-input [input]
  (map (fn [s] ((case (first s) \L -, \R +) (parse-long (subs s 1))))
       (string/split-lines input)))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn digit-sequence [xs] (reductions (fn [x y] (mod (+ x y) 100)) 50 xs))

(defn count-zeros [xs] (count (filter #{0} xs)))

(def answer-part-1 (comp count-zeros digit-sequence))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 3]
  [:puzzle 962])


;;;; Part 2

(defn expand-ticks [xs]
  (mapcat (fn [x] (repeat (abs x) (if (pos? x) 1 -1))) xs))

(def answer-part-2 (comp count-zeros digit-sequence expand-ticks))

(core/set-answer-fn! 2 answer-part-2
  [:sample1 6]
  [:puzzle 5782])
