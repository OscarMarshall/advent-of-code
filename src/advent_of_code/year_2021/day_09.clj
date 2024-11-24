(ns advent-of-code.year-2021.day-09
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2021 9)


;;;; Parse

(defn parse-input [input]
  (mapv (partial mapv (comp parse-long str)) (string/split-lines input)))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn neighbor-posns [heightmap posn]
  (for [offset [[-1 0] [0 -1] [0 1] [1 0]]
        :let   [neighbor-posn (map + posn offset)]
        :when  (get-in heightmap neighbor-posn)]
    neighbor-posn))

(defn neighbors [heightmap posn]
  (map (partial get-in heightmap) (neighbor-posns heightmap posn)))

(defn low-points [heightmap]
  (for [x     (range (count heightmap))
        y     (range (count (first heightmap)))
        :let  [posn [x y]
               height (get-in heightmap posn)]
        :when (every? (partial < height) (neighbors heightmap posn))]
    posn))

(defn answer-part-1 [parsed-input]
  (transduce (map (comp inc (partial get-in parsed-input)))
             +
             (low-points parsed-input)))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 550])


;;;; Part 2

(defn higher-neighbors [heightmap posn]
  (filter (comp (every-pred (complement #{9})
                            (partial < (get-in heightmap posn)))
                (partial get-in heightmap))
          (neighbor-posns heightmap posn)))

(defn basin-size [heightmap posn]
  (loop [[current-posn & to-explore] (list posn), seen #{}]
    (if (nil? current-posn)
      (count seen)
      (recur (cond-> to-explore
               (not (seen current-posn)) (into (higher-neighbors heightmap
                                                                 current-posn)))
             (conj seen current-posn)))))

(defn answer-part-2 [parsed-input]
  (->> parsed-input
       low-points
       (map (partial basin-size parsed-input))
       sort
       (take-last 3)
       (apply *)))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 1100682])
