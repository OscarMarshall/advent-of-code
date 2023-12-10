(ns advent-of-code.year-2023.day-10
  (:require [advent-of-code.core :as core]
            [clojure.math :as math]
            [clojure.string :as str]
            [medley.core :as medley]))

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

(def vector->pipe
  {[-1 0] \|
   [1 0]  \|
   [0 -1] \-
   [0 1]  \-})

(defn extend-pipe [grid coordinates]
  (reduce (fn [grid vector]
            (assoc-in grid
                      (mapv + coordinates vector)
                      (vector->pipe vector)))
          grid
          (pipe->neighbor-vectors (get-in grid coordinates))))

(defn extend-pipes [grid]
  (transduce (remove #(#{\. \S} (get-in grid %)))
             (completing extend-pipe)
             grid
             (all-coordinates grid)))

(defn expand-grid [grid]
  (let [empty-row (vec (repeat (* (count (first grid)) 2) \.))]
    (extend-pipes (vec (interleave (mapv #(vec (interleave % (repeat \.))) grid)
                                   (repeat empty-row))))))

(defn tile-neighbors [coordinates]
  (map #(mapv + coordinates %) [[-1 0] [0 1] [1 0] [0 -1]]))

(defn ground-coordinates? [grid coordinates] (= (get-in grid coordinates) \.))

(defn outside-tiles
  ([grid]
   (let [grid-height      (count grid)
         grid-width       (count (first grid))
         rows             (range grid-height)
         columns          (range grid-width)
         edge-coordinates (concat (map vector (repeat -1) columns)
                                  (map vector (repeat grid-height) columns)
                                  (map vector rows (repeat -1))
                                  (map vector rows (repeat grid-width)))]
     (outside-tiles grid
                    (medley/queue edge-coordinates)
                    (set edge-coordinates))))
  ([grid queue seen]
   (if (empty? queue)
     seen
     (let [neighbors (filter #(ground-coordinates? grid %)
                             (tile-neighbors (peek queue)))]
       (recur grid
              (into (pop queue) (remove seen neighbors))
              (into seen neighbors))))))

(defn answer-part-2 [grid]
  (let [simplified-grid     (simplify-grid grid)
        expanded-grid       (expand-grid simplified-grid)
        outside-coordinates (into #{}
                                  (map (fn [coordinates] (mapv #(/ % 2)
                                                               coordinates))
                                       (outside-tiles expanded-grid)))]
    (count (sequence (comp (remove outside-coordinates)
                           (filter #(ground-coordinates? simplified-grid %)))
                     (all-coordinates simplified-grid)))))

(core/part 2
    parse-input answer-part-2 *file*
    [:sample3 4]
    [:sample4 8]
    [:sample5 10]
    [:input 443])
