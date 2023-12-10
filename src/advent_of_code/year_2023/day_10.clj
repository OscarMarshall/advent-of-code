(ns advent-of-code.year-2023.day-10
  (:require [advent-of-code.core :as core]
            [clojure.math :as math]
            [clojure.string :as str]))

(println "# Day 10")

(set! *warn-on-reflection* true)

(defn parse-input [input] (mapv vec (str/split-lines input)))

;;;; Part 1

(defn all-coordinates [grid]
  (let [columns (range (count (first grid)))]
    (for [row    (range (count grid))
          column columns]
      [row column])))

(def pipe->neighbor-vectors
  {\| [[-1 0] [1 0]]
   \- [[0 -1] [0 1]]
   \L [[-1 0] [0 1]]
   \J [[-1 0] [0 -1]]
   \7 [[0 -1] [1 0]]
   \F [[0 1] [1 0]]
   \. []
   \S [[-1 0] [0 1] [1 0] [0 -1]]})

(defn neighbors [grid coordinates]
  (into #{}
        (map #(mapv + coordinates %))
        (pipe->neighbor-vectors (get-in grid coordinates))))

(defn connected? [grid coordinates1 coordinates2]
  (and ((neighbors grid coordinates1) coordinates2)
       ((neighbors grid coordinates2) coordinates1)))

(defn trace [grid previous current]
  (cons current
        (lazy-seq (when-some [next (first (disj (neighbors grid current)
                                                previous))]
                    (trace grid current next)))))

(defn start-coordinates [grid]
  (first (filter (fn [coordinates] (= (get-in grid coordinates) \S))
                 (all-coordinates grid))))

(defn answer-part-1 [grid]
  (let [start (start-coordinates grid)]
    (transduce (comp (map (fn [neighbor]
                            (take-while #(not (= (get-in grid %) \S))
                                        (trace grid start neighbor))))
                     (filter #(connected? grid (last %) start))
                     (map #(long (math/ceil (/ (count %) 2)))))
               max
               0
               (neighbors grid start))))

(core/part 1
  parse-input answer-part-1 *file*
  [:sample1 4]
  [:sample2 8]
  [:input 6979])


;;;; Part 2

(defn answer-part-2 [x]
  x)

(core/part 2
  parse-input answer-part-2 *file*
  #_[:sample1]
  [:input])
