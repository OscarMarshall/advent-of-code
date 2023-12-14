(ns advent-of-code.year-2023.day-14
  (:require [advent-of-code.core :as core]
            [clojure.string :as str]
            [medley.core :as medley]))

(println "# Day 14")

(set! *warn-on-reflection* true)

(defn parse-input [input] (mapv vec (str/split-lines input)))

;;;; Part 1

(defn rotate-grid-cw [grid n]
  (if (zero? n)
    grid
    (recur (apply mapv #(vec (reverse %&)) grid) (dec n))))

(defn tilt-row-left [row]
  (vec (mapcat #(reverse (sort %)) (medley/partition-after #{\#} row))))

(defn tilt-grid-left [grid]
  (mapv tilt-row-left grid))

(defn total-load [grid]
  (apply + (map (fn [row distance-from-south-edge]
                  (* (count (filter #{\O} row)) distance-from-south-edge))
                (reverse grid) (range 1 ##Inf))))

(defn answer-part-1 [grid]
  (total-load (rotate-grid-cw (tilt-grid-left (rotate-grid-cw grid 3)) 1)))

(core/part 1
  parse-input answer-part-1 *file*
  [:sample1 136]
  [:input 112048])


;;;; Part 2

(def spin-cycle
  (memoize
   (fn [grid]
     (rotate-grid-cw (nth (iterate #(rotate-grid-cw (tilt-grid-left %) 1)
                                   (rotate-grid-cw grid 3))
                          4)
                     1))))

(defn answer-part-2 [grid]
  (loop [grid grid, spins 1000000000, seen {}]
    (if (zero? spins) (total-load grid)
      (if-some [previous-spins (seen grid)]
        (recur grid (long (rem spins (- previous-spins spins))) {})
        (recur (spin-cycle grid) (dec spins) (assoc seen grid spins))))))

(core/part 2
  parse-input answer-part-2 *file*
  [:sample1 64]
  [:input 105606])
