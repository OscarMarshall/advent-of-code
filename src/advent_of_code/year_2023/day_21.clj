(ns advent-of-code.year-2023.day-21
  (:require [advent-of-code.core :as core]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [medley.core :as medley]))

(println "# Day 21")

(set! *warn-on-reflection* true)

(defn parse-input [input] (mapv vec (str/split-lines input)))

;;;; Part 1

(def garden-plots #{\. \S})

(defn all-locations [chart]
  (map vec (combo/cartesian-product (range (count chart))
                                    (range (count (first chart))))))

(defn starting-location [chart]
  (medley/find-first #(= (get-in chart %) \S) (all-locations chart)))

(def neighbor-vectors [[-1 0] [0 1] [1 0] [0 -1]])

(defn possible-steps [chart location]
  (eduction (map #(mapv + location %))
            (filter #(garden-plots (get-in chart %)))
            neighbor-vectors))

(defn possible-locations [chart]
  (iterate (fn [locations]
             (into #{}
                   (mapcat #(possible-steps chart %))
                   locations))
           #{(starting-location chart)}))

(defn answer-part-1 [chart]
  (count (nth (possible-locations chart) 64)))

(core/part 1
  parse-input answer-part-1 *file*
  [:input 3768])


;;;; Part 2

(defn answer-part-2 [x]
  x)

(core/part 2
  parse-input answer-part-2 *file*
  [:input #_(core/current-answer 2)])
