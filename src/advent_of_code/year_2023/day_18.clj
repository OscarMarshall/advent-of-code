(ns advent-of-code.year-2023.day-18
  (:require [advent-of-code.core :as core]
            [clojure.string :as str]
            [medley.core :as medley]))

(println "# Day 18")

(set! *warn-on-reflection* true)

(def string->direction
  {"U" :up
   "D" :down
   "L" :left
   "R" :right})

(defn parse-input [input]
  (map (fn [[_ direction-string distance color]]
         [(string->direction direction-string) (parse-long distance) color])
       (re-seq #"([UDLR]) (\d+) \((.*)\)" input)))

;;;; Part 1

(def direction->vector
  {:up    [-1 0]
   :right [0 1]
   :down  [1 0]
   :left  [0 -1]})

(defn next-coordinates [coordinates direction]
  (mapv + coordinates (direction->vector direction)))

(defn dig
  ([plans] (into #{} q(dig plans [0 0])))
  ([[[direction distance] & plans] coordinates]
   (when direction
     (let [dug-coordinates (take distance
                                 (rest (iterate #(next-coordinates % direction)
                                                coordinates)))]
       (concat dug-coordinates (dig plans (last dug-coordinates)))))))

(defn neighbors [coordinates]
  (into #{} (map #(mapv + coordinates %)) [[-1 0] [0 -1] [0 1] [1 0]]))

(defn dug-out-size [coordinates]
  (let [top                 (apply min (map first coordinates))
        left                (apply min (map second coordinates))
        bottom              (apply max (map first coordinates))
        right               (apply max (map second coordinates))
        columns             (range left (inc right))
        rows                (range top (inc bottom))
        edge-coordinates    (concat (map vector (repeat -1) columns)
                                    (map vector (repeat (inc bottom)) columns)
                                    (map vector rows (repeat -1))
                                    (map vector rows (repeat (inc right))))
        in-range?           (fn [[row column]]
                              (and (<= left column right)
                                   (<= top row bottom)))
        outside-coordinates (loop [queue (medley/queue edge-coordinates)
                                   seen  (set edge-coordinates)]
                              (if (empty? queue)
                                seen
                                (let [neighbors
                                      (filter #(and (in-range? %)
                                                    (not (coordinates %)))
                                              (neighbors (peek queue)))]
                                  (recur (into (pop queue)
                                               (remove seen neighbors))
                                         (into seen neighbors)))))]
    (apply + (for [row    rows
                   column columns
                   :when  (not (outside-coordinates [row column]))]
               1))))

(defn answer-part-1 [plans]
  (dug-out-size (dig plans)))

(core/part 1
  parse-input answer-part-1 *file*
  [:sample1 62]
  [:input 7494])


;;;; Part 2

(defn answer-part-2 [x]
  x)

(core/part 2
  parse-input answer-part-2 *file*
  [:sample1 #_?]
  [:input #_(core/current-answer 2)])
