(ns advent-of-code.year-2024.day-25
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]
            [clojure.math.combinatorics :as combo]))

(set! *warn-on-reflection* true)

(core/set-date! 2024 25)


;;;; Parse

(defn parse-input [input]
  (-> input
      (string/split #"\n\n")
      (->> (map (fn [s]
                  [(if (= (first s) \#) :locks :keys)
                   (apply map
                          (fn [& cs] (dec (count (filter #{\#} cs))))
                          (string/split-lines s))]))
           (group-by first))
      (update-vals (partial map second))))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn answer-part-1 [{:keys [locks keys]}]
  (count (filter (fn [[lock key]] (every? #(<= % 5) (map + lock key)))
                 (combo/cartesian-product locks keys))))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 3]
  [:puzzle #_(get-in @core/state [:part1 :outputs :puzzle :result])])


;;;; Part 2

(defn answer-part-2 [x]
  x)

#_(core/set-answer-fn! 2 answer-part-2
    [:sample1]
    [:puzzle #_(get-in @core/state [:part2 :outputs :puzzle :result])])
