(ns advent-of-code.year-2023.day-10
  (:require [advent-of-code.core :as core]
            [clojure.math :as math]
            [clojure.string :as str]
            [medley.core :as medley]
            [clojure.set :as set]))

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
  {\| #{[-1 0] [1 0]}
   \- #{[0 -1] [0 1]}
   \L #{[-1 0] [0 1]}
   \J #{[-1 0] [0 -1]}
   \7 #{[0 -1] [1 0]}
   \F #{[0 1] [1 0]}
   \. #{}
   \S #{[-1 0] [0 1] [1 0] [0 -1]}})

(def neighbor-vectors->pipe (set/map-invert pipe->neighbor-vectors))

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
  (first (filter #(= (get-in grid %) \S) (all-coordinates grid))))

(defn find-loop [grid]
  (let [start (start-coordinates grid)]
    (cons start
          (first (eduction (filter #(connected? grid % start))
                           (map (fn [neighbor]
                                  (take-while #(not (= (get-in grid %) \S))
                                              (trace grid start neighbor))))
                           (filter #(connected? grid (last %) start))
                           (neighbors grid start))))))

(defn answer-part-1 [grid] (/ (count (find-loop grid)) 2))

(core/part 1
  parse-input answer-part-1 *file*
  [:sample1 4]
  [:sample2 8]
  [:input 6979])


;;;; Part 2

(defn simplify-grid [grid]
  (let [loop-coordinates (into #{} (find-loop grid))]
    (reduce #(assoc-in %1 %2 \.)
            grid
            (remove loop-coordinates (all-coordinates grid)))))

(defn replace-s [grid]
  (let [start      (start-coordinates grid)
        start-pipe (neighbor-vectors->pipe
                    (into #{}
                          (filter #(connected? grid start (mapv + start %)))
                          (pipe->neighbor-vectors \S)))]
    (assoc-in grid start start-pipe)))

(defn count-inner-tiles [grid]
  (transduce cat
             (fn
               ([[count]] count)
               ([[count inside last-elbow] character]
                (case character
                  \.      [(cond-> count inside inc) inside]
                  \|      [count (not inside)]
                  (\L \F) [count inside character]
                  \J      (if (= last-elbow \F)
                            [count (not inside)]
                            [count inside])
                  \7      (if (= last-elbow \L)
                            [count (not inside)]
                            [count inside])
                  [count inside last-elbow])))
             [0 false]
             (replace-s (simplify-grid grid))))

(defn answer-part-2 [grid] (count-inner-tiles grid))

(core/part 2
    parse-input answer-part-2 *file*
    [:sample3 4]
    [:sample4 8]
    [:sample5 10]
    [:input 443])
