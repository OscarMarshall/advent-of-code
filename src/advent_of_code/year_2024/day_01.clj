(ns advent-of-code.year-2024.day-01
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2024 1)


;;;; Parse

(defn parse-input [input]
  (->> input
       string/split-lines
       (map #(map parse-long (string/split % #" +")))
       (apply map vector)))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn answer-part-1 [id-lists]
  (apply + (apply map (comp abs -) (map sort id-lists))))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 11]
  [:puzzle 2344935])


;;;; Part 2

(defn answer-part-2 [[left-list right-list]]
  (let [similarity-scores (into {}
                                (map (fn [[id frequency]] [id (* id frequency)]))
                                (frequencies right-list))]
    (transduce (map (fn [id] (similarity-scores id 0)))
               +
               left-list)))

(core/set-answer-fn! 2 answer-part-2
  [:sample1 31]
  [:puzzle 27647262])
