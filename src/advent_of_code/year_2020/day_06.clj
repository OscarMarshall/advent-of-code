(ns advent-of-code.year-2020.day-06
  (:require [advent-of-code.core :as core]
            [clojure.set :as set]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2020 6)


;;;; Parse

(defn parse-input [input]
  (map (comp (partial map set) string/split-lines)
       (string/split input #"\n\n")))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn reduce-groups [reduce-members parsed-input]
  (transduce (map (comp count reduce-members)) + parsed-input))

(defn answer-part-1 [parsed-input]
  (reduce-groups (partial apply set/union) parsed-input))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 6596])


;;;; Part 2

(defn answer-part-2 [parsed-input]
  (reduce-groups (partial apply set/intersection) parsed-input))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 3219])
