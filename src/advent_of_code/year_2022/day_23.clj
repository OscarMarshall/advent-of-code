(ns advent-of-code.year-2022.day-23
  (:require [advent-of-code.core :as core]
            [clojure.core.match :refer [match]]
            [clojure.string :as string]
            [medley.core :as medley]))

(def input (core/get-input))

(defn parse-input [input]
  (let [lines (string/split-lines input)]
    (into #{} (for [column (range (count (first lines)))
                    row    (range (count lines))
                    :let   [position [row column]]
                    :when  (= (get-in lines position) \#)]
                position))))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

(defn elf-window [[row column] elves]
  (mapv (fn [row]
          (mapv (fn [column] (if (contains? elves [row column]) 1 0))
                (range (dec column) (+ column 2))))
        (range (dec row) (+ row 2))))

(defmulti can-propose? (fn [_ direction] direction))
(defmethod can-propose? :north [window _]
  (match window
    [[0 0 0]
     [_ _ _]
     [_ _ _]] true
    :else false))
(defmethod can-propose? :south [window _]
  (match window
    [[_ _ _]
     [_ _ _]
     [0 0 0]] true
    :else false))
(defmethod can-propose? :west [window _]
  (match window
    [[0 _ _]
     [0 _ _]
     [0 _ _]] true
    :else false))
(defmethod can-propose? :east [window _]
  (match window
    [[_ _ 0]
     [_ _ 0]
     [_ _ 0]] true
    :else false))

(defn move [[row column] direction]
  (case direction
    :north [(dec row) column]
    :south [(inc row) column]
    :west  [row (dec column)]
    :east  [row (inc column)]
    [row column]))

(defn propose-move [position directions elves]
  (let [window (elf-window position elves)]
    (match window
      [[0 0 0]
       [0 _ 0]
       [0 0 0]] nil
      :else (move position
                  (medley/find-first
                   (partial can-propose? window)
                   (take 4 directions))))))

(defn step [[elves directions]]
  [(into #{}
         (mapcat (fn [[to froms]] (if (= (count froms) 1) [to] froms)))
         (group-by #(propose-move % directions elves) elves))
   (rest directions)])

(defn rounds [elves] (iterate step [elves (cycle [:north :south :west :east])]))

(defn answer-part-1 [elves]
  (let [[elves] (nth (rounds elves) 10)]
    (- (transduce (map (fn [f]
                         (let [points (into (sorted-set) (map f) elves)]
                           (inc (- (last points) (first points))))))
                  *
                  [first second])
       (count elves))))

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 4049))


;;; Part 2
;;; ============================================================================

(defn answer-part-2 [elves]
  (count (medley/take-upto (partial apply =)
                           (partition 2 1 (map first (rounds elves))))))

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer 1021))
