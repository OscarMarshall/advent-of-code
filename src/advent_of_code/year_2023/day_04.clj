(ns advent-of-code.year-2023.day-04
  (:require [advent-of-code.core :as core]
            [clojure.set :as set]
            [clojure.string :as str]
            [medley.core :as medley]))

(println "# Day 4")

(set! *warn-on-reflection* true)

(defn parse-numbers [s] (map parse-long (re-seq #"\d+" s)))

(defn parse-card [card]
  (let [[_ id body] (re-matches #"Card +(\d+): +(.*)" card)
        id          (parse-long id)]
    (assoc (zipmap [:winning-numbers :numbers]
                   (map #(set (parse-numbers %))
                        (str/split body #" +\| +")))
           :id id)))

(defn parse-input [input]
  (medley/index-by :id (map parse-card (str/split-lines input))))


;;;; Part 1

(def scores (vec (take 11 (cons 0 (iterate #(* % 2) 1)))))

(defn matched-numbers [{:keys [winning-numbers numbers]}]
  (count (set/intersection winning-numbers numbers)))

(defn score-card [card] (nth scores (matched-numbers card)))

(defn answer-part-1 [cards]
  (apply + (map score-card (vals cards))))

(core/part 1
  parse-input answer-part-1 *file*
  [:sample1 13]
  [:input 15268])


;;;; Part 2

(defn initial-card-counts [cards] (medley/map-vals #(assoc % :count 1) cards))

(defn add-copies [cards ids amount]
  (reduce #(update-in %1 [%2 :count] + amount) cards ids))

(defn cards-won [{:as card, :keys [id]}]
  (take (matched-numbers card) (range (inc id) ##Inf)))

(defn process-cards [cards]
  (reduce (fn [cards id]
            (let [{:as card, :keys [count]} (cards id)]
              (add-copies cards (cards-won card) count)))
          (initial-card-counts cards)
          (range 1 (inc (count cards)))))

(defn answer-part-2 [cards]
  (transduce (map :count) + (vals (process-cards cards))))

(core/part 2
  parse-input answer-part-2 *file*
  [:sample1 30]
  [:input 6283755])
