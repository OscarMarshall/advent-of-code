(ns advent-of-code.year-2023.day-24
  (:require [advent-of-code.core :as core]
            [clojure.string :as str]
            [numeric.expresso.core :as expresso]
            [clojure.math.combinatorics :as combo]))

(println "# Day 24")

(set! *warn-on-reflection* true)

(defn parse-input [input]
  (map (fn [line]
         (map #(mapv parse-long (str/split % #", ")) (str/split line #" @ ")))
       (str/split-lines input)))

;;;; Part 1

(defn hailstone-paths-cross? [[[[x1 y1 _] [xv1 yv1 _]] [[x2 y2 _] [xv2 yv2 _]]]]
  (let [{:syms [ax ay u v]}
        (first (expresso/solve
                '[ax ay u v]
                (expresso/ex (= ax (+ ~(bigint x1) (* u ~(bigint xv1)))))
                (expresso/ex (= ay (+ ~(bigint y1) (* u ~(bigint yv1)))))
                (expresso/ex (= bx (+ ~(bigint x2) (* v ~(bigint xv2)))))
                (expresso/ex (= by (+ ~(bigint y2) (* v ~(bigint yv2)))))
                (expresso/ex (= ax bx))
                (expresso/ex (= ay by))))]
    (and (some? ax)
         (<= 200000000000000 ax 400000000000000)
         (<= 200000000000000 ay 400000000000000)
         (>= u 0)
         (>= v 0))))

(defn answer-part-1 [hailstones]
  (count (filter hailstone-paths-cross? (combo/combinations hailstones 2))))

(core/part 1
  parse-input answer-part-1 *file*
  [:input 12015])


;;;; Part 2

(defn answer-part-2 [x]
  x)

(core/part 2
  parse-input answer-part-2 *file*
  [:input #_(core/current-answer 2)])
