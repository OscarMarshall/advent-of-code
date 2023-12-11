(ns advent-of-code.year-2023.day-11
  (:require [advent-of-code.core :as core]
            [clojure.string :as str]))

(println "# Day 11")

(set! *warn-on-reflection* true)

(defn parse-input [input] (mapv vec (str/split-lines input)))

;;;; Part 1

(defn image->galaxies [image]
  (let [columns (range (count (first image)))]
    (set (for [row    (range (count image))
               column columns
               :let   [coordinates [row column]]
               :when  (= (get-in image [row column]) \#)]
           coordinates))))

(defn expand-dimension [galaxies dimension rate]
  (let [coordinate->galaxies (group-by #(nth % dimension) galaxies)
        new-rows             (dec rate)]
    (first (reduce (fn [[galaxies expansion] index]
                     (if-some [coordinate-galaxies (coordinate->galaxies index)]
                       [(reduce (fn [galaxies galaxy]
                                  (conj galaxies
                                        (update galaxy dimension + expansion)))
                                galaxies
                                coordinate-galaxies)
                        expansion]
                       [galaxies (+ expansion new-rows)]))
                   [#{} 0]
                   (range (inc (apply max (keys coordinate->galaxies))))))))

(defn expand-galaxies [galaxies rate]
  (reduce #(expand-dimension %1 %2 rate) galaxies (range 2)))

(defn sum-all-distances [galaxies]
  (let [galaxies       (vec galaxies)
        galaxies-count (count galaxies)]
    (apply + (for [index1 (range galaxies-count)
                   index2 (range (inc index1) galaxies-count)]
               (apply + (apply map
                               #(abs (- %1 %2))
                               (map galaxies [index1 index2])))))))

(defn answer-part-1 [image]
  (sum-all-distances (expand-galaxies (image->galaxies image) 2)))

(core/part 1
  parse-input answer-part-1 *file*
  [:sample1 374]
  [:input 9681886])


;;;; Part 2

(defn answer-part-2 [image]
  (sum-all-distances (expand-galaxies (image->galaxies image) 1000000)))

(core/part 2
  parse-input answer-part-2 *file*
  [:sample1 82000210]
  [:input 791134099634])
