(ns advent-of-code.year-2023.day-07
  (:require [advent-of-code.core :as core]
            [clojure.string :as str]))

(def input (core/get-input))

(defn parse-input [input]
  (map #(update (zipmap [:hand :bid] (str/split % #" ")) :bid parse-long)
       (str/split-lines input)))

(def parsed-input (parse-input input))


;;;; Part 1

(def character->strength
  (into {} (map vector
                (concat (map #(first (str %)) (range 2 10)) [\T \J \Q \K \A])
                (range))))

(def hand-type->strength
  (into {} (map vector [:high-card
                        :one-pair
                        :two-pair
                        :three-of-a-kind
                        :full-house
                        :four-of-a-kind
                        :five-of-a-kind]
                (range))))

(defn hand-type [hand]
  (let [[x y] (reverse (sort (vals (frequencies hand))))]
    (case x
      5 :five-of-a-kind
      4 :four-of-a-kind
      3 (if (= y 2) :full-house :three-of-a-kind)
      2 (if (= y 2) :two-pair :one-pair)
      1 :high-card)))

(defn answer-part-1 [hands-and-bids]
  (reduce +
          (sequence (map (fn [{:keys [bid]} rank] (* bid rank)))
                    (sort-by (fn [{:keys [hand]}]
                               [(hand-type->strength (hand-type hand))
                                (mapv character->strength hand)])
                             hands-and-bids)
                    (range 1 ##Inf))))

(def part-1-answer (answer-part-1 parsed-input))

(assert (> part-1-answer 253046761))
(assert (= part-1-answer 253205868))


;;;; Part 2

(defn answer-part-2 [x]
  x)

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer part-2-answer))
