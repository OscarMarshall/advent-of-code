(ns advent-of-code.year-2020.day-17
  (:require [advent-of-code-2020.core :as core]
            [advent-of-code-2020.utils :as utils]
            [clojure.string :as string]))

(def input (core/get-input))

(defn parse-input [input]
  (string/split-lines input))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

(defn parsed-input->active-cubes [parsed-input dimensions]
  (into #{} (for [x     (range (count parsed-input))
                  y     (range (count (first parsed-input)))
                  :when (= (get-in parsed-input [x y]) \#)]
              (vec (take dimensions (concat [x y] (repeat 0)))))))

(defn neighbors+self [posn dimensions]
  (if (zero? dimensions)
    '([])
    (let [dimension-coord (peek posn)
          lower-dimension (neighbors+self (pop posn) (dec dimensions))]
      (mapcat (fn [x] (map #(conj % x) lower-dimension))
              (range (dec dimension-coord) (+ dimension-coord 2))))))

(defn neighbors [posn dimensions]
  (remove #{posn} (neighbors+self posn dimensions)))

(defn execute-cycle-fn [dimensions]
  (utils/conway-step-fn #(neighbors % dimensions) #{3} #{2 3}))

(defn answer-part-1 [parsed-input]
  (count (nth (iterate (execute-cycle-fn 3)
                       (parsed-input->active-cubes parsed-input 3))
              6)))

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 313))


;;; Part 2
;;; ============================================================================

(defn answer-part-2 [parsed-input]
  (count (nth (iterate (execute-cycle-fn 4)
                       (parsed-input->active-cubes parsed-input 4))
              6)))

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer 2640))
