(ns advent-of-code.year-2023.day-03
  (:require [advent-of-code.core :as core]
            [clojure.string :as str]))

(def input (core/get-input))

(defn parse-input [input] (mapv vec (str/split-lines input)))

(def parsed-input (parse-input input))


;;;; Part 1

(def digits (set (map (comp first str) (range 10))))

(defn schematic-width [schematic] (count (first schematic)))

(defn schematic-height [schematic] (count schematic))

(defn number-length [[row column] schematic]
  (count (take-while digits (subvec (schematic row) column))))

(defn number-neighbor-coordinates [[row column :as coordinates] schematic]
  (let [column-start (dec column)
        column-end   (+ column (number-length coordinates schematic))
        column-range (range column-start (inc column-end))]
    (conj
     ;; xxxxx
     ;; .nnn.
     ;; xxxxx
     (for [row2 [(dec row) (inc row)], column2 column-range] [row2 column2])
     [row column-start]                 ; xnnn.
     [row column-end])))                ; .nnnx

(defn symbol-character? [character]
  (not (or (digits character) (#{\.} character))))

(defn part-number-coordinate? [coordinates schematic]
  (boolean (some #(symbol-character? (get-in schematic % \.))
                 (number-neighbor-coordinates coordinates schematic))))

(defn all-coordinates [schematic]
  (let [widths (range (schematic-width schematic))]
    (for [row    (range (schematic-height schematic))
          column widths]
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
                             (+ column (number-length coordinate schematic))))))

(defn answer-part-1 [schematic]
  (transduce (map #(coordinate->part-number % schematic))
             +
             (part-number-coordinates schematic)))

(def part-1-answer (answer-part-1 parsed-input))

(assert (not= part-1-answer 291177))
(assert (not= part-1-answer 534693))
(assert (= part-1-answer 528819))


;;;; Part 2

(defn asterisk-coordinates [schematic]
  (filter #(#{\*} (get-in schematic %))
          (all-coordinates schematic)))

(defn part-number-coordinates-by-neighbor [schematic]
  (transduce (map (fn [coordinates]
              (zipmap (number-neighbor-coordinates coordinates schematic)
                      (repeat [(coordinate->part-number coordinates
                                                        schematic)]))))
             (partial merge-with into)
             (part-number-coordinates schematic)))

(defn gear-ratio [coordinates part-number-neighbors]
  (let [neighbors (part-number-neighbors coordinates)]
    (when (= (count neighbors) 2)
      (apply * neighbors))))

(defn answer-part-2 [schematic]
  (let [part-number-neighbors (part-number-coordinates-by-neighbor schematic)]
    (transduce (keep #(gear-ratio % part-number-neighbors))
               +
               (asterisk-coordinates schematic))))

(def part-2-answer (answer-part-2 parsed-input))

(assert (not= part-2-answer 232350))
(assert (= part-2-answer 80403602))
