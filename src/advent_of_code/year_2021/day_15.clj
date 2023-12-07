(ns advent-of-code.year-2021.day-15
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(def input (core/get-input *file*))

(defn parse-input [input]
  (mapv (partial mapv (fn [c] (Long/parseLong (str c))))
        (string/split-lines input)))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

(defn neighbors [risk-level-grid posn]
  (eduction (map (partial mapv + posn))
            (filter (partial get-in risk-level-grid))
            '([-1 0] [0 -1] [1 0] [0 1])))

(defn lowest-risk-level [risk-level-grid start destination]
  (loop [to-visit (sorted-set [0 start])
         seen     #{}]
    (let [[total-risk posn :as entry] (first to-visit)
          to-visit                    (disj to-visit entry)]
      (if (= posn destination)
        total-risk
        (recur (cond-> to-visit
                 (not (seen posn))
                 (into (comp (remove seen)
                             (map (juxt (comp (partial + total-risk)
                                              (partial get-in risk-level-grid))
                                        identity)))
                       (neighbors risk-level-grid posn)))
               (conj seen posn))))))

(defn answer-part-1 [parsed-input]
  (lowest-risk-level parsed-input
                     [0 0]
                     (vec (repeat 2 (dec (count parsed-input))))))

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 472))


;;; Part 2
;;; ============================================================================

(def succ (zipmap (range 1 10) (rest (cycle (range 1 10)))))

(defn full-grid [grid]
  (into [] cat (take 5 (iterate (partial map (partial mapv succ))
                                (map (comp (partial into [] cat)
                                           (partial take 5)
                                           (partial iterate (partial map succ)))
                                     grid)))))

(defn answer-part-2 [parsed-input]
  (lowest-risk-level (full-grid parsed-input)
                     [0 0]
                     (vec (repeat 2 (dec (* (count parsed-input) 5))))))

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer 2851))
