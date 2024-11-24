(ns advent-of-code.year-2021.day-17
  (:require [advent-of-code.core :as core]))

(set! *warn-on-reflection* true)

(core/set-date! 2021 17)


;;;; Parse

(defn parse-input [input]
  (let [[x1 x2] (map parse-long (rest (re-find #"x=(-?\d+)\.\.(-?\d+)" input)))
        [y1 y2] (map parse-long (rest (re-find #"y=(-?\d+)\.\.(-?\d+)" input)))]
    {:x-range [x1 x2]
     :y-range [y1 y2]}))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn y-posns [velocity]
  (map first (iterate (fn [[y velocity]] [(+ y velocity) (dec velocity)])
                      [0 velocity])))

(defn highest-y-position [y-velocity]
  (reduce (fn [acc y] (if (= acc y) (reduced acc) y)) (y-posns y-velocity)))

(defn largest-y-velocity [y-target] (dec (- y-target)))

(defn answer-part-1 [{:keys [y-range]}]
  (highest-y-position (largest-y-velocity (apply min y-range))))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 5778])


;;;; Part 2

(defn triangle-number [n] (/ (* n (+ n 1)) 2))

(def triangle-numbers (map triangle-number (range)))

(def n-triangle-number
  (memoize
   (fn [x]
     (transduce (map-indexed vector)
                (completing (fn [_ [i y]]
                              (cond
                                (= y x) (reduced i)
                                (> y x) (reduced nil))))
                nil
                triangle-numbers))))

(defn find-x-velocity [target steps]
  (assert (pos? target) "Negative x targets not implemented")
  (or (let [drag-count          (dec steps)
            drag-removed-target (- target (triangle-number drag-count))]
        (when (pos? drag-removed-target)
          (let [result (+ (/ drag-removed-target steps) drag-count)]
            (when (integer? result) result))))
      (when-let [n (n-triangle-number target)]
        (when (<= n steps) n))))

(defn find-y-velocity [target steps]
  (let [drag-count (dec steps)
        result     (+ (/ (- target (triangle-number drag-count)) steps)
                      drag-count)]
    (when (integer? result) result)))

(defn answer-part-2 [{:keys [x-range y-range]}]
  (count (into #{} (for [y     (range (first y-range) (inc (second y-range)))
                         :let  [most-steps (+ (* (largest-y-velocity y) 2) 2)]
                         steps (range 1 (inc most-steps))
                         :let  [y-velocity (find-y-velocity y steps)]
                         :when y-velocity
                         x     (range (first x-range) (inc (second x-range)))
                         :let  [x-velocity (find-x-velocity x steps)]
                         :when x-velocity]
                     [x-velocity y-velocity]))))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 2576])
