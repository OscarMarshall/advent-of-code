(ns advent-of-code.year-2023.day-14
  (:require [advent-of-code.core :as core]
            [clojure.string :as str]
            [medley.core :as medley]))

(println "# Day 14")

(set! *warn-on-reflection* true)

(defn parse-input [input] (mapv vec (str/split-lines input)))

(comment
  (core/current-parsed-input :sample1)
  (core/current-parsed-input)
  )

;;;; Part 1

(defn reflect-grid [grid]
  (apply mapv vector grid))

(defn tilt-row-left [row]
  (vec (mapcat #(reverse (sort %)) (medley/partition-after #{\#} row))))

(defn tilt-grid-left [grid]
  (mapv tilt-row-left grid))

(defn total-load [grid]
  (apply + (map (fn [row distance-from-south-edge]
                  (* (count (filter #{\O} row)) distance-from-south-edge))
                (reverse grid) (range 1 ##Inf))))

(defn answer-part-1 [grid]
  (total-load (reflect-grid (tilt-grid-left (reflect-grid grid)))))

(core/part 1
  parse-input answer-part-1 *file*
  [:sample1 136]
  [:input 112048])

(comment
  (core/current-answer 1 :sample1)
  )


;;;; Part 2

(defn answer-part-2 [x]
  x)

(core/part 2
  parse-input answer-part-2 *file*
  [:sample1 #_?]
  [:input #_(core/current-answer 2)])
