(ns advent-of-code.year-2023.day-24
  (:require [advent-of-code.core :as core]
            [clojure.core.matrix :as matrix]
            [clojure.core.matrix.linear :as linear]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]
            [numeric.expresso.core :as expresso]))

(println "# Day 24")

(set! *warn-on-reflection* true)

(matrix/set-current-implementation :vectorz)

(defn parse-input [input]
  (map (fn [line]
         (map #(mapv bigint (str/split % #", ")) (str/split line #" @ ")))
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

;;; Gave up and stole
;;; https://github.com/sigttou/aoc/blob/437662f50cf49c3d2ed23c2e5f660dab9acb9596/aoc-2023/src/aoc_2023/day_24.clj
(defn solve-3-stones
  [[[[x0 y0 z0] [vx0 vy0 vz0]]
    [[x1 y1 z1] [vx1 vy1 vz1]]
    [[x2 y2 z2] [vx2 vy2 vz2]]]]
  (let [eq    [[0 (- vz0 vz1) (- vy1 vy0) 0 (- z1 z0) (- y0 y1)]
               [(- vz1 vz0) 0 (- vx0 vx1) (- z0 z1) 0 (- x1 x0)]
               [(- vy0 vy1) (- vx1 vx0) 0 (- y1 y0) (- x0 x1) 0]
               [0 (- vz0 vz2) (- vy2 vy0) 0 (- z2 z0) (- y0 y2)]
               [(- vz2 vz0) 0 (- vx0 vx2) (- z0 z2) 0 (- x2 x0)]
               [(- vy0 vy2) (- vx2 vx0) 0 (- y2 y0) (- x0 x2) 0]]
        idpx0 (- (* y0 vz0) (* vy0 z0))
        idpx1 (- (* y1 vz1) (* vy1 z1))
        idpx2 (- (* y2 vz2) (* vy2 z2))
        idpy0 (- (* z0 vx0) (* vz0 x0))
        idpy1 (- (* z1 vx1) (* vz1 x1))
        idpy2 (- (* z2 vx2) (* vz2 x2))
        idpz0 (- (* x0 vy0) (* vx0 y0))
        idpz1 (- (* x1 vy1) (* vx1 y1))
        idpz2 (- (* x2 vy2) (* vx2 y2))
        sol   [(- idpx0 idpx1)
               (- idpy0 idpy1)
               (- idpz0 idpz1)
               (- idpx0 idpx2)
               (- idpy0 idpy2)
               (- idpz0 idpz2)]]
    (linear/solve eq sol)))

(defn answer-part-2 [hailstones]
  (->> hailstones
       (take-last 3)
       solve-3-stones
       (take 3)
       (apply +')
       long))

(core/part 2
  parse-input answer-part-2 *file*
  [:input
   [> 1016365642179115]
   [= 1016365642179116]])
