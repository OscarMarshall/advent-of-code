(ns advent-of-code.year-2025.day-04
  (:require
   [advent-of-code.core :as core]
   [advent-of-code.grid :as grid]))

(set! *warn-on-reflection* true)

(core/set-date! 2025 4)


;;;; Parse

(def parse-input #(grid/parse % {\@ :roll}))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn process-grid [grid]
  (let [rolls (get-in grid [:indices :roll])]
    (into {}
          (map (fn [coordinates]
                 [coordinates
                  (set (filter rolls (grid/adjacents coordinates)))]))
          rolls)))

(defn moveable-rolls [state]
  (eduction (keep (fn [[coordinates adjacents]]
                    (when (< (count adjacents) 4) coordinates)))
            state))

(def answer-part-1 (comp count seq moveable-rolls process-grid))

(core/set-answer-fn! 1 answer-part-1 [:sample1 13] [:puzzle 1505])


;;;; Part 2

(defn remove-roll [state roll]
  (reduce (fn [state coordinates]
            (let [state (update state coordinates disj roll)]
              (cond-> state
                (< (count (state coordinates)) 4) (remove-roll coordinates))))
          (dissoc state roll)
          (state roll)))

(defn answer-part-2 [grid]
  (let [first-state (process-grid grid)
        last-state (reduce remove-roll
                           first-state
                           (moveable-rolls first-state))]
    (- (count first-state) (count last-state))))

(core/set-answer-fn! 2 answer-part-2 [:sample1 43] [:puzzle 9182])
