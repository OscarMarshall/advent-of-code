(ns advent-of-code-2020.day-10
  (:require [advent-of-code-2020.core :as core]
            [clojure.string :as string]))

;;; Part 1
;;; ============================================================================

(def input (core/get-input))

(defn parse-input [input]
  (map #(Long/parseLong %) (string/split-lines input)))

(def parsed-input (parse-input input))

(defn answer-part-1 [parsed-input]
  (let [joltage-adapters   (sort parsed-input)
        joltage-adapters   (concat (cons 0 joltage-adapters)
                                   [(+ (last joltage-adapters) 3)])
        {ones 1, threes 3} (frequencies (map #(- %2 %1)
                                             joltage-adapters
                                             (rest joltage-adapters)))]
    (* ones threes)))

(def part-1-answer (answer-part-1 parsed-input))

(comment
  part-1-answer
  ;; => 2040
  )

;;; Part 2
;;; ============================================================================

(def count-arrangements
  (memoize
   (fn [[a b :as joltage-adapters] prev]
     (if (nil? a)
       1
       (let [joltage-adapters (rest joltage-adapters)]
         (cond-> (count-arrangements joltage-adapters a)
           (<= (- (or b (+ a 3)) prev) 3)
           (+ (count-arrangements joltage-adapters prev))))))))

(defn answer-part-2 [parsed-input]
  (count-arrangements (sort parsed-input) 0))

(def part-2-answer (answer-part-2 parsed-input))

(comment
  part-2-answer
  ;; => 28346956187648
  )
