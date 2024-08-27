(ns advent-of-code.year-2023.day-16
  (:require [advent-of-code.core :as core]
            [clojure.string :as str]))

(println "# Day 16")

(set! *warn-on-reflection* true)

(defn parse-input [input] (mapv vec (str/split-lines input)))

;;;; Part 1

(def character->direction->next-directions
  {\. {:up    [:up]
       :right [:right]
       :down  [:down]
       :left  [:left]}
   \/ {:up    [:right]
       :right [:up]
       :down  [:left]
       :left  [:down]}
   \\ {:up    [:left]
       :right [:down]
       :down  [:right]
       :left  [:up]}
   \| {:up    [:up]
       :right [:up :down]
       :down  [:down]
       :left  [:up :down]}
   \- {:up    [:left :right]
       :right [:right]
       :down  [:left :right]
       :left  [:left]}})

(def direction->vector
  {:up    [-1 0]
   :right [0 1]
   :down  [1 0]
   :left  [0 -1]})

(defn next-coordinates [coordinates direction]
  (mapv + coordinates (direction->vector direction)))

(defn energized-tiles [layout starting-beam]
  (loop [beams [starting-beam], seen #{}]
    (if-some [[coordinates direction :as beam] (first beams)]
      (if (seen beam)
        (recur (rest beams) seen)
        (let [character       (get-in layout coordinates)
              next-directions (get-in character->direction->next-directions
                                      [character direction])]
          (recur (into (rest beams)
                       (comp (map (fn [direction]
                                    [(next-coordinates coordinates direction)
                                     direction]))
                             (filter #(get-in layout (first %))))
                       next-directions)
                 (conj seen beam))))
      (count (distinct (map first seen))))))

(defn answer-part-1 [layout] (energized-tiles layout [[0 0] :right]))

(core/part 1
  parse-input answer-part-1 *file*
  [:sample1 46]
  [:input 7046])


;;;; Part 2

(defn answer-part-2 [layout]
  (let [height (count layout)
        width  (count (first layout))]
    (transduce (comp cat
                     (map #(energized-tiles layout %)))
               max
               0
               (concat (map (fn [row]
                              [[[row 0] :right] [[row (dec width)] :right]])
                            (range height))
                       (map (fn [column]
                              [[[0 column] :down] [[(dec height) column] :up]])
                            (range width))))))

(core/part 2
  parse-input answer-part-2 *file*
  [:sample1 51]
  [:input 7313])
