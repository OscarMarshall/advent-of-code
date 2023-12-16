(ns advent-of-code.year-2023.day-16
  (:require [advent-of-code.core :as core]
            [clojure.string :as str]))

(println "# Day 16")

(set! *warn-on-reflection* true)

(defn parse-input [input] (mapv vec (str/split-lines input)))

(comment
  (core/current-parsed-input :sample1)
  (core/current-parsed-input)
  )

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

(defn next-coordinate [coordinates direction]
  (mapv + coordinates (direction->vector direction)))

(defn answer-part-1 [layout]
  (loop [beams [[[0 0] :right]], seen #{}]
    (if-some [[coordinates direction :as beam] (first beams)]
      (if (seen beam)
        (recur (rest beams) seen)
        (let [character       (get-in layout coordinates)
              next-directions (get-in character->direction->next-directions
                                      [character direction])]
          (recur (into (rest beams)
                       (comp (map (fn [direction]
                                    [(next-coordinate coordinates direction)
                                     direction]))
                             (filter #(get-in layout (first %))))
                       next-directions)
                 (conj seen beam))))
      (count (distinct (map first seen))))))

(core/part 1
  parse-input answer-part-1 *file*
  [:sample1 46]
  [:input 7046])


;;;; Part 2

(defn answer-part-2 [x]
  x)

(core/part 2
  parse-input answer-part-2 *file*
  [:sample1 #_51]
  [:input #_(core/current-answer 2)])
