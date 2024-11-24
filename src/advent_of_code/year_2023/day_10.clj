(ns advent-of-code.year-2023.day-10
  (:require [advent-of-code.core :as core]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]
            [clojure.string :as str]
            [medley.core :as medley]))

(set! *warn-on-reflection* true)

(core/set-date! 2023 10)


;;;; Parse

(defn parse-input [input] (mapv vec (str/split-lines input)))

(core/set-parse-fn! parse-input)

;;;; Part 1

(defn all-coordinates [grid]
  (map vec (combo/cartesian-product (range (count grid))
                                    (range (count (first grid))))))

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
  (boolean (and ((neighbors grid coordinates1) coordinates2)
                ((neighbors grid coordinates2) coordinates1))))

(defn trace [grid previous current]
  (cons current
        (lazy-seq (when-some [next (first (disj (neighbors grid current)
                                                previous))]
                    (trace grid current next)))))

(defn start-coordinates [grid]
  (medley/find-first #(= (get-in grid %) \S) (all-coordinates grid)))

(defn find-loop [grid]
  (let [start (start-coordinates grid)]
    (cons start
          (medley/find-first
           #(connected? grid (last %) start)
           (eduction (filter #(connected? grid % start))
                     (map (fn [neighbor]
                            (take-while #(not (= (get-in grid %) \S))
                                        (trace grid start neighbor))))
                     (neighbors grid start))))))

(defn answer-part-1 [grid] (/ (count (find-loop grid)) 2))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 4]
  [:sample2 8]
  [:puzzle 6979])


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
  (first (reduce
          (fn [[count inside] [x y]]
            (or (case x
                  \. [(cond-> count inside inc) inside]
                  \| [count (not inside)]
                  \L (when (= \7 y) [count (not inside)])
                  \F (when (= \J y) [count (not inside)])
                  nil)
                [count inside]))
          [0 false]
          (take-while seq
                      (iterate rest
                               (sequence (comp cat (remove #{\-}))
                                         (replace-s (simplify-grid grid))))))))

(defn answer-part-2 [grid] (count-inner-tiles grid))

(core/set-answer-fn! 2 answer-part-2
    [:sample3 4]
    [:sample4 8]
    [:sample5 10]
    [:puzzle 443])
