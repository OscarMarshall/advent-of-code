(ns advent-of-code.year-2021.day-11
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2021 11)


;;;; Parse

(defn parse-input [input]
  (mapv (partial mapv (comp parse-long str)) (string/split-lines input)))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn all-posns [grid]
  (for [x (range (count grid)), y (range (count (first grid)))] [x y]))

(defn neighbor-posns [grid [x y :as posn]]
  (for [neighbor-x (range (max (dec x) 0) (min (+ x 2) (count grid)))
        neighbor-y (range (max (dec y) 0) (min (+ y 2) (count (first grid))))
        :let       [neighbor-posn [neighbor-x neighbor-y]]
        :when      (not= neighbor-posn posn)]
    neighbor-posn))

(defn step [grid]
  (let [all-posns (all-posns grid)]
    (loop [grid (mapv (partial mapv inc) grid)]
      (if-let [flashing-posns (seq (filter (comp (partial < 9)
                                                 (partial get-in grid))
                                          all-posns))]
        (recur (reduce (fn [grid flashing-posn]
                         (reduce (fn [grid neighbor-posn]
                                   (update-in grid
                                              neighbor-posn
                                              (fn [x] (cond-> x (pos? x) inc))))
                                 (assoc-in grid flashing-posn 0)
                                 (neighbor-posns grid flashing-posn)))
                       grid
                       flashing-posns))
        grid))))

(defn answer-part-1 [parsed-input]
  (count (filter zero? (flatten (take 101 (iterate step parsed-input))))))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 1585])


;;;; Part 2

(defn answer-part-2 [parsed-input]
  (count (take-while (comp (partial not-every? #{0}) flatten)
                     (iterate step parsed-input))))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 382])
