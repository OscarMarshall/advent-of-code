(ns advent-of-code.year-2023.day-07
  (:require [advent-of-code.core :as core]
            [clojure.string :as str]))

(println "# Day 7")

(set! *warn-on-reflection* true)

(defn parse-input [input]
  (map #(update (zipmap [:hand :bid] (str/split % #" ")) :bid parse-long)
       (str/split-lines input)))


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
    (case (long x)
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

(core/part 1
  parse-input answer-part-1 *file*
  [:sample1 6440]
  [:input [> 253046761] 253205868])


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

(core/part 2
  parse-input answer-part-2 *file*
  [:sample1 5905]
  [:input 253907829])
