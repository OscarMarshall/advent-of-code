(ns advent-of-code.year-2019.day-01
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2019 1)


;;;; Parse

(defn parse-input [input] (map parse-long (string/split-lines input)))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn fuel-required [mass]
  (- (quot mass 3) 2))

(defn answer-part-1 [module-masses]
  (transduce (map fuel-required) + module-masses))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 3224048])


;;;; Part 2

(defn fuel-required2 [mass]
  (transduce (comp (drop 1) (take-while pos?)) + (iterate fuel-required mass)))

(defn answer-part-2 [module-masses]
  (transduce (map fuel-required2) + module-masses))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 4833211])
