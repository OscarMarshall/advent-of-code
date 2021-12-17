(ns advent-of-code.year-2020.day-11
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(def input (core/get-input))

(defn parse-input [input]
  (mapv vec (string/split-lines input)))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

(defn find-neighbors [layout x y]
  (for [x2    (range (dec x) (+ x 2))
        y2    (range (dec y) (+ y 2))
        :when (or (not= x2 x) (not= y2 y))
        :when (= (get-in layout [x2 y2]) \L)]
    [x2 y2]))

(defn make-seat-graph [layout find-neighbors]
  (into {}
        (for [x     (range (count layout))
              y     (range (count (first layout)))
              :when (= (get-in layout [x y]) \L)]
          [[x y]
           {:taken     false
            :neighbors (find-neighbors layout x y)}])))

(defn at-least? [pred n coll]
  (>= (reduce (fn [acc x]
                (if (pred x)
                  (let [acc (inc acc)]
                    (cond-> acc
                      (>= acc n) reduced))
                  acc))
              0
              coll)
      n))

(defn step [seat-graph step-seat]
  (into {}
        (map (juxt key (comp (fn [{:as seat, :keys [neighbors]}]
                               (let [neighbors (map seat-graph neighbors)]
                                 (step-seat seat neighbors)))
                             val)))
        seat-graph))

(defn step-seat [{:as seat, :keys [taken]} neighbors]
  (cond-> seat
    (if taken
      (at-least? :taken 4 neighbors)
      (every? (comp not :taken) neighbors))
    (update :taken not)))

(defn stable-state [seat-graph step-seat]
  (loop [seat-graph seat-graph]
    (let [next-seat-graph (step seat-graph step-seat)]
      (cond-> next-seat-graph
        (not= next-seat-graph seat-graph) recur))))

(defn answer-part-1 [parsed-input]
  (let [seat-graph (make-seat-graph parsed-input find-neighbors)]
    (count (filter :taken (vals (stable-state seat-graph step-seat))))))

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 2494))


;;; Part 2
;;; ============================================================================

(defn find-neighbors2 [layout x y]
  (for [dx    (range -1 2)
        dy    (range -1 2)
        :when (not= dx dy 0)
        :let  [posn (loop [x2 (+ x dx), y2 (+ y dy)]
                     (case (get-in layout [x2 y2])
                       \. (recur (+ x2 dx)
                                 (+ y2 dy))
                       \L [x2 y2]
                       nil))]
        :when posn]
    posn))

(defn step-seat2 [{:as seat, :keys [taken]} neighbors]
  (cond-> seat
    (if taken
      (at-least? :taken 5 neighbors)
      (every? (comp not :taken) neighbors))
    (update :taken not)))

(defn answer-part-2 [parsed-input]
  (let [seat-graph (make-seat-graph parsed-input find-neighbors2)]
    (count (filter :taken (vals (stable-state seat-graph step-seat2))))))

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer 2306))
