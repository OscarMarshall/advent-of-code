(ns advent-of-code.year-2023.day-03
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

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
        column-end   (+ column (number-length coordinates schematic))]
    (concat (for [row2    [(dec row) (inc row)]
                  column2 (range column-start (inc column-end))]
              [row2 column2])
            (map (fn [column2] [row column2])
                 (range column-start (inc column-end))))))

(defn part-number-coordinate? [coordinates schematic]
  (some (fn [coordinates2]
          (not ((conj digits \.) (get-in schematic coordinates2))))
        (number-neighbor-coordinates coordinates schematic)))

(defn all-coordinates [schematic]
  (for [row    (range (schematic-height schematic))
        column (range (schematic-width schematic))]
    [row column]))

(defn number-coordinates [schematic]
  (filter (fn [[row column :as coordinates]]
            (and (digits (get-in schematic coordinates))
                 (not (digits (get-in schematic [row (dec column)])))))
          (all-coordinates schematic)))

(defn coordinate->part-number [[row column :as coordinate] schematic]
  (parse-long (apply str
                     (subvec (schematic row)
                             column
                             (+ column
                                (part-number-length coordinate schematic))))))

(defn answer-part-1 [schematic]
  (apply + (map #(coordinate->part-number % schematic)
                (filter #(part-number-coordinate? % schematic)
                        (number-coordinates schematic)))))

(def part-1-answer (answer-part-1 parsed-input))

(assert (not= part-1-answer 291177))
(assert (= part-1-answer 534693))


;;; Part 2
;;; ============================================================================

(defn answer-part-2 [parsed-input]
  parsed-input)

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer part-2-answer))
