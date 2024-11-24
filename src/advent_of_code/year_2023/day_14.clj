(ns advent-of-code.year-2023.day-14
  (:require [advent-of-code.core :as core]
            [clojure.string :as str]
            [medley.core :as medley]))

(set! *warn-on-reflection* true)

(core/set-date! 2023 14)


;;;; Parse

(defn parse-input [input] (str/split-lines input))

(core/set-parse-fn! parse-input)

;;;; Part 1

(defn rotate-grid-cw [grid n]
  (if (zero? n)
    grid
    (recur (apply mapv #(reverse %&) grid) (dec n))))

(defn tilt-row-left [row]
  (mapcat #(sort-by {\O 0, \. 1, \# 2} %) (medley/partition-after #{\#} row)))

(defn tilt-grid-left [grid]
  (mapv tilt-row-left grid))

(defn total-load [grid]
  (apply + (map (fn [row distance-from-south-edge]
                  (* (count (filter #{\O} row)) distance-from-south-edge))
                (reverse grid) (range 1 ##Inf))))

(defn answer-part-1 [grid]
  (total-load (rotate-grid-cw (tilt-grid-left (rotate-grid-cw grid 3)) 1)))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 136]
  [:puzzle 112048])


;;;; Part 2

(def spin-cycle
  (memoize
   (fn [grid]
     (rotate-grid-cw (nth (iterate #(rotate-grid-cw (tilt-grid-left %) 1)
                                   (rotate-grid-cw grid 3))
                          4)
                     1))))

(defn answer-part-2 [grid]
  (loop [[grid & remaining-grids :as grids] (iterate spin-cycle grid)
         spins                              1000000000
         seen                               {}]
    (if (zero? spins)
      (total-load grid)
      (if-some [previous-spins (seen grid)]
        (recur grids (long (rem spins (- previous-spins spins))) {})
        (recur remaining-grids (dec spins) (assoc seen grid spins))))))

(core/set-answer-fn! 2 answer-part-2
  [:sample1 64]
  [:puzzle 105606])
