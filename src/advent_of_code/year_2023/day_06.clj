(ns advent-of-code.year-2023.day-06
  (:require [advent-of-code.core :as core]
            [clojure.string :as str]))

(def input (core/get-input))

(defn parse-input [input]
  (apply map
         (fn [time distance] {:time time, :distance distance})
         (map #(map parse-long (re-seq #"\d+" %)) (str/split-lines input))))

(def parsed-input (parse-input input))


;;;; Part 1

(defn ways-faster [{:keys [time distance]}]
  (count (filter #(> % distance)
                 (map (fn [press-time] (* press-time (- time press-time)))
                      (range time)))))

(defn answer-part-1 [x]
  (transduce (map ways-faster) * x))

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 861300))


;;;; Part 2

(defn answer-part-2 [x]
  x)

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer part-2-answer))
