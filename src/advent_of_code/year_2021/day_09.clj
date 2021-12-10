(ns advent-of-code.year-2021.day-09
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(def input (core/get-input))

(defn parse-input [input]
  (mapv (partial mapv (comp #(Long/parseLong %) str))
        (string/split-lines input)))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

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

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 550))


;;; Part 2
;;; ============================================================================

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

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer 1100682))
