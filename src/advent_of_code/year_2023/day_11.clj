(ns advent-of-code.year-2023.day-11
  (:require [advent-of-code.core :as core]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)

(core/set-date! 2023 11)


;;;; Parse

(defn parse-input [input] (mapv vec (str/split-lines input)))

(core/set-parse-fn! parse-input)

;;;; Part 1

(defn image->galaxies [image]
  (into []
        (comp (filter #(= (get-in image %) \#)) (map vec))
        (combo/cartesian-product (range (count image))
                                 (range (count (first image))))))

(defn expand-dimension [galaxies dimension rate]
  (let [coordinate->galaxies (group-by #(nth % dimension) galaxies)
        new-rows             (dec rate)]
    (transduce (map coordinate->galaxies)
               (fn
                 ([[galaxies]] galaxies)
                 ([[galaxies expansion] coordinate-galaxies]
                  (if coordinate-galaxies
                    [(reduce (fn [galaxies galaxy]
                               (conj galaxies (update galaxy
                                                      dimension
                                                      +
                                                      expansion)))
                             galaxies
                             coordinate-galaxies)
                     expansion]
                    [galaxies (+ expansion new-rows)])))
               [[] 0]
               (range (inc (apply max (keys coordinate->galaxies)))))))

(defn expand-galaxies [galaxies rate]
  (reduce #(expand-dimension %1 %2 rate) galaxies (range 2)))

(defn galaxy-distance [galaxy1 galaxy2]
  (apply + (map #(abs (- %1 %2)) galaxy1 galaxy2)))

(defn sum-all-distances [galaxies]
  (apply + (map #(apply galaxy-distance %) (combo/combinations galaxies 2))))

(defn answer-part-1 [image]
  (sum-all-distances (expand-galaxies (image->galaxies image) 2)))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 374]
  [:puzzle 9681886])


;;;; Part 2

(defn answer-part-2 [image]
  (sum-all-distances (expand-galaxies (image->galaxies image) 1000000)))

(core/set-answer-fn! 2 answer-part-2
  [:sample1 82000210]
  [:puzzle 791134099634])
