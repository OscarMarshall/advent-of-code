(ns advent-of-code.year-2023.day-03
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]
            [clojure.set :as set]))

(def input (core/get-input))

(defn parse-input [input] (mapv vec (string/split-lines input)))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

(def digits (set (map (comp first str) (range 10))))

(defn schematic-width [schematic] (count (first schematic)))

(defn schematic-height [schematic] (count schematic))

(defn number-length [[row column] schematic]
  (count (take-while digits (map #(get-in schematic [row %])
                                 (range column (schematic-width schematic))))))

(defn number-neighbor-coordinates [[row column :as coordinates] schematic]
  (let [column-start (dec column)
        column-end   (+ column (number-length coordinates schematic))
        column-range (range column-start (inc column-end))]
    (conj (for [row2 [(dec row) (inc row)], column2 column-range]
            [row2 column2])
          [row column-start]
          [row column-end])))

(defn part-number-coordinate? [coordinates schematic]
  (some (fn [coordinates2]
          (not ((conj digits \.) (get-in schematic coordinates2 \.))))
        (number-neighbor-coordinates coordinates schematic)))

(defn all-coordinates [schematic]
  (let [width (schematic-width schematic)]
    (for [row    (range (schematic-height schematic))
          column (range width)]
      [row column])))

(defn number-coordinates [schematic]
  (filter (fn [[row column :as coordinates]]
            (and (digits (get-in schematic coordinates))
                 (not (digits (get-in schematic [row (dec column)])))))
          (all-coordinates schematic)))

(defn part-number-coordinates [schematic]
  (filter #(part-number-coordinate? % schematic)
          (number-coordinates schematic)))

(defn coordinate->part-number [[row column :as coordinate] schematic]
  (parse-long (apply str
                     (subvec (schematic row)
                             column
                             (+ column
                                (number-length coordinate schematic))))))

(defn answer-part-1 [schematic]
  (apply + (map #(coordinate->part-number % schematic)
                (part-number-coordinates schematic))))

(def part-1-answer (answer-part-1 parsed-input))

(assert (not= part-1-answer 291177))
(assert (not= part-1-answer 534693))
(assert (= part-1-answer 528819))


;;; Part 2
;;; ============================================================================

(defn asterisk-coordinates [schematic]
  (filter (fn [coordinates] (= (get-in schematic coordinates) \*))
          (all-coordinates schematic)))

(defn part-number-coordinates-by-neighbor [schematic]
  (->> (part-number-coordinates schematic)
       (map (fn [coordinates]
              (zipmap (number-neighbor-coordinates coordinates schematic)
                      (repeat coordinates))))
       (apply merge-with set/union)))

(defn gear-ratio [coordinates coordinates->part-number-neighbors]
  (let [neighbors (coordinates->part-number-neighbors coordinates)]
    (when (= (count neighbors) 2)
      (apply * neighbors))))

(defn answer-part-2 [schematic]
  (let [coordinates->part-number-neighbors (part-number-coordinates-by-neighbor
                                            schematic)]
    (apply + (keep #(gear-ratio % coordinates->part-number-neighbors)
                   (asterisk-coordinates schematic)))))

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer 232350))
