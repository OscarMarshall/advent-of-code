(ns advent-of-code.year-2023.day-18
  (:require [advent-of-code.core :as core]))

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
  {:up    [0 -1]
   :right [1 0]
   :down  [0 1]
   :left  [-1 0]})

(defn next-coordinates [coordinates direction]
  (mapv + coordinates (direction->vector direction)))

(defn dig [plans]
  (reduce (fn [cuts [direction distance]]
            (let [start (or (peek (peek cuts)) [0 0])]
              (conj cuts
                    [start
                     (mapv +
                           start
                           (mapv *
                                 (repeat distance)
                                 (direction->vector direction)))])))
          []
          plans))

(defn neighbors [coordinates]
  (into #{} (map #(mapv + coordinates %)) [[-1 0] [0 -1] [0 1] [1 0]]))

(defn cut-left [[[_ x] [_ _]]] x)
(defn cut-right [[[_ _] [_ x]]] x)
(defn cut-top [[[x _] [_ _]]] x)

(defn remove-horizontal-range [cuts
                               [[row1 column1] [_ column2] :as cut]
                               left
                               right]
  (into (disj cuts cut)
        (concat (when (< column1 left) [[[row1 column1] [row1 (dec left)]]])
                (when (> column2 right)
                  [[[row1 (inc right)] [row1 column2]]]))))

(defn dug-out-size [cuts]
  (transduce (map (fn [[[x1 y1] [x2 y2]]]
                    (+ (- (* x1 y2) (* x2 y1)) (abs (+ (- x2 x1) (- y2 y1))))))
             (completing + #(inc (/ % 2)))
             cuts))

(defn answer-part-1 [plans]
  (dug-out-size (dig plans)))

(core/part 1
  parse-input answer-part-1 *file*
  [:sample1 62]
  [:input [not= 7494] 36725])


;;;; Part 2

(def hex-string->direction
  {"3" :up
   "1" :down
   "2" :left
   "0" :right})

(defn answer-part-2 [plans]
  (answer-part-1 (map (fn [[_ _ color]]
                        [(hex-string->direction (subs color 6))
                         (Long/parseLong (subs color 1 6) 16)])
                      plans)))

(core/part 2
  parse-input answer-part-2 *file*
  [:sample1 952408144115]
  [:input 97874103749720])
