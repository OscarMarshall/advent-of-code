(ns advent-of-code.year-2023.day-04
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]
            [clojure.set :as set]))

(def input (core/get-input))

(defn parse-numbers [s] (map parse-long (string/split s #" +")))

(defn parse-card [card]
  (let [[_ id body] (re-matches #"Card +(\d+): +(.*)" card)]
    [(parse-long id) (zipmap [:winning-numbers :numbers]
                             (map (comp set parse-numbers)
                                  (string/split body #" \| ")))]))

(defn parse-input [input]
  (into {} (map parse-card) (string/split-lines input)))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

(def scores (cons 0 (iterate (partial * 2) 1)))

(defn score-card [{:keys [winning-numbers numbers]}]
  (nth scores (count (set/intersection winning-numbers numbers))))

(defn answer-part-1 [parsed-input]
  (apply + (map score-card (vals parsed-input))))

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 15268))


;;; Part 2
;;; ============================================================================

(defn answer-part-2 [parsed-input]
  parsed-input)

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer part-2-answer))
