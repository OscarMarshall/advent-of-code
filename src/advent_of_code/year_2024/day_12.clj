(ns advent-of-code.year-2024.day-12
  (:require [advent-of-code.core :as core]
            [advent-of-code.utils :as utils]))

(set! *warn-on-reflection* true)

(core/set-date! 2024 12)


;;;; Parse

(defn parse-input [input] (:pois (utils/parse-2d-map input identity)))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn neighbors [[x y]] [[(inc x) y] [x (inc y)] [(dec x) y] [x (dec y)]])

(defn garden-plot-info [garden-plot-map position]
  (let [{siblings true, edges false}
        (group-by (comp boolean #{(garden-plot-map position)} garden-plot-map)
                  (neighbors position))]
    [(into #{} siblings)
     (map (fn [edge-position] [position edge-position]) edges)]))

(defn region-info [garden-plot-map position]
  (loop [[position & positions] (list position), seen #{}, fences 0]
    (cond
      (nil? position) [(* (count seen) fences) seen]
      (seen position) (recur positions seen fences)
      :else           (let [[siblings edges] (garden-plot-info garden-plot-map
                                                               position)]
                        (recur (into positions siblings)
                               (conj seen position)
                               (+ fences (count edges)))))))

(defn answer-part-1 [garden-plot-map]
  (->> garden-plot-map
       keys
       (reduce (fn [[price seen] position]
                 (if (seen position)
                   [price seen]
                   (let [[new-price positions] (region-info garden-plot-map
                                                            position)]
                     [(+ price new-price) (into seen positions)])))
               [0 #{}])
       first))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 140]
  [:sample2 772]
  [:sample3 1930]
  [:puzzle 1437300])


;;;; Part 2

(defn new-fences [garden-plot-map position]
  (let [plant                (garden-plot-map position)
        [right down left up] (map garden-plot-map (neighbors position))]
    (+ (if (and (not= up plant)
                (or (not= left plant)
                    (= (garden-plot-map (map + position [-1 -1])) plant)))
         1
         0)
       (if (and (not= right plant)
                (or (not= up plant)
                    (= (garden-plot-map (map + position [1 -1])) plant)))
         1
         0)
       (if (and (not= down plant)
                (or (not= left plant)
                    (= (garden-plot-map (map + position [-1 1])) plant)))
         1
         0)
       (if (and (not= left plant)
                (or (not= up plant)
                    (= (garden-plot-map (map + position [-1 -1])) plant)))
         1
         0))))

(defn region-info-bulk [garden-plot-map position]
  (loop [[position & positions] (list position), seen #{}, fences 0]
    (cond
      (nil? position) [(* (count seen) fences) seen]
      (seen position) (recur positions seen fences)
      :else           (let [siblings (filter (comp #{(garden-plot-map position)}
                                                   garden-plot-map)
                                             (neighbors position))]
                        (recur (into positions siblings)
                               (conj seen position)
                               (+ fences
                                  (new-fences garden-plot-map position)))))))

(defn answer-part-2 [garden-plot-map]
  (->> garden-plot-map
       keys
       (reduce (fn [[price seen] position]
                 (if (seen position)
                   [price seen]
                   (let [[new-price positions] (region-info-bulk garden-plot-map
                                                                 position)]
                     [(+ price new-price) (into seen positions)])))
               [0 #{}])
       first))

(core/set-answer-fn! 2 answer-part-2
  [:sample1 80]
  [:sample2 436]
  [:sample3 1206]
  [:sample4 236]
  [:sample5 368]
  [:puzzle 849332])
