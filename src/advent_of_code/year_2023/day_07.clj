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

(defn hand-strength [hand]
  [(hand-type->strength (hand-type hand)) (mapv character->strength hand)])

(defn score-hands-and-bids [sorted-hands-and-bids]
  (reduce + (sequence (map (fn [{:keys [bid]} rank] (* bid rank)))
                      sorted-hands-and-bids
                      (range 1 ##Inf))))

(defn answer-part-1 [hands-and-bids]
  (score-hands-and-bids (sort-by (fn [{:keys [hand]}] (hand-strength hand))
                                 hands-and-bids)))

(def part-1-answer (answer-part-1 parsed-input))

(assert (> part-1-answer 253046761))
(assert (= part-1-answer 253205868))


;;;; Part 2

(def character->strength2
  (into {} (map vector
                (concat [\J] (map #(first (str %)) (range 2 10)) [\T \Q \K \A])
                (range))))

(defn hand-type2 [hand]
  (let [{:as kinds} (frequencies hand)]
    (hand-type (str/replace hand
                            \J
                            (key (apply max-key val (assoc kinds \J 0)))))))

(defn hand-strength2 [hand]
  [(hand-type->strength (hand-type2 hand)) (mapv character->strength2 hand)])

(defn answer-part-2 [hands-and-bids]
  (score-hands-and-bids (sort-by (fn [{:keys [hand]}] (hand-strength2 hand))
                                 hands-and-bids)))

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer 253907829))
