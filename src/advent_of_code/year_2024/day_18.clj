(ns advent-of-code.year-2024.day-18
  (:require [advent-of-code.core :as core]
            [advent-of-code.utils :as utils]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2024 18)


;;;; Parse

(defn parse-input [input]
  (map (comp (partial mapv parse-long) rest) (re-seq #"(\d+),(\d+)" input)))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn neighbors [[x y]]
  (filter (partial every? #(<= 0 % 70))
          [[(inc x) y] [x (inc y)] [(dec x) y] [x (dec y)]]))

(defn minimum-steps-after-wait [falling-bytes wait]
  (let [bytes (into #{} (take wait) falling-bytes)]
    (->> [0 0]
         (utils/djkstra-walk (fn [coordinates]
                               (remove bytes (neighbors coordinates))))
         (some (fn [[coordinates steps]]
                 (when (= coordinates [70 70]) steps))))))

(defn answer-part-1 [falling-bytes]
  (minimum-steps-after-wait falling-bytes 1024))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 374])


;;;; Part 2

(defn answer-part-2 [falling-bytes]
  (loop [lower-bound 0, upper-bound (count falling-bytes)]
    (let [midpoint (quot (+ lower-bound upper-bound) 2)]
      (if (minimum-steps-after-wait falling-bytes midpoint)
        (if (= midpoint (dec upper-bound))
          #break (string/join "," (nth falling-bytes (dec upper-bound)))
          (recur midpoint upper-bound))
        (recur lower-bound midpoint)))))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle [not= "6,42"] [not= "2,27"] "30,12"])
