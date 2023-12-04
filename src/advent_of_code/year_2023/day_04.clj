(ns advent-of-code.year-2023.day-04
  (:require [advent-of-code.core :as core]
            [clojure.set :as set]
            [clojure.string :as string]
            [medley.core :as medley]))

(def input (core/get-input))

(defn parse-numbers [s] (map parse-long (string/split s #" +")))

(defn parse-card [card]
  (let [[_ id body] (re-matches #"Card +(\d+): +(.*)" card)
        id          (parse-long id)]
    (assoc (zipmap [:winning-numbers :numbers]
                   (map (comp set parse-numbers)
                        (string/split body #" +\| +")))
           :id id)))

(defn parse-input [input]
  (medley/index-by :id (map parse-card (string/split-lines input))))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

(def scores (vec (take 11 (cons 0 (iterate (partial * 2) 1)))))

(defn matched-numbers [{:keys [winning-numbers numbers]}]
  (count (set/intersection winning-numbers numbers)))

(defn score-card [card] (nth scores (matched-numbers card)))

(defn answer-part-1 [parsed-input]
  (apply + (map score-card (vals parsed-input))))

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 15268))


;;; Part 2
;;; ============================================================================

(defn initial-card-counts [cards]
  (medley/map-vals (fn [v] (assoc v :count 1)) cards))

(defn add-copies [cards ids amount]
  (reduce #(update-in %1 [%2 :count] + amount) cards ids))

(defn cards-won [{:as card, :keys [id]}]
  (take (matched-numbers card) (range (inc id) ##Inf)))

(defn answer-part-2 [cards]
  (apply + (map :count
                (vals (reduce (fn [cards id]
                                (let [{:as card, :keys [count]} (cards id)]
                                  (add-copies cards (cards-won card) count)))
                              (initial-card-counts cards)
                              (range 1 (inc (count cards))))))))

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer 6283755))
