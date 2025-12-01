(ns advent-of-code.year-2024.day-13
  (:require [advent-of-code.core :as core]
            [advent-of-code.utils :as utils]
            [clojure.string :as string]
            [medley.core :as medley]
            [clojure.math :as math]))

(set! *warn-on-reflection* true)

(core/set-date! 2024 13)


;;;; Parse

(defn parse-input [input]
  (->> input
       (re-seq #"Button A: X\+(\d+), Y\+(\d+)\nButton B: X\+(\d+), Y\+(\d+)\nPrize: X=(\d+), Y=(\d+)")
       (map (fn [match]
              (let [[ax ay bx by x y] (map parse-long (rest match))]
                {:a-button [ax ay], :b-button [bx by], :target [x y]})))))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn tokens-to-win [{[ax ay] :a-button, [bx by] :b-button, [tx ty] :target}]
  (let [a-presses (/ (- (* bx ty) (* by tx)) (- (* bx ay) (* by ax)))
        b-presses (/ (- (* ax ty) (* ay tx)) (- (* ax by) (* ay bx)))]
    (when (and (integer? a-presses) (integer? b-presses))
      (+ (* a-presses 3) b-presses))))

(defn tokens-to-win* [{[ax ay] :a-button, [bx by] :b-button, [tx ty] :target}]
  (loop [[a-low a-high] (cond-> [0 (inc (quot tx ax))]
                          (> (* by (quot tx bx)) ty) reverse)]
    (let [a-presses  (quot (+ a-low a-high) 2)
          partial-tx (- tx (* ax a-presses))
          b-presses  (quot partial-tx bx)
          current-y  (+ (* ay a-presses) (* by b-presses))]
      (cond
        (= current-y ty)            (when (= (+ (* ax a-presses)
                                                (* bx b-presses))
                                             tx)
                                      (+ (* 3 a-presses) b-presses))
        (#{a-low a-high} a-presses) nil
        (< current-y ty)            (recur [a-presses a-high])
        :else                       (recur [a-low a-presses])))))

(defn answer-part-1 [machines]
  (transduce (keep tokens-to-win) + machines)
  #_(keep #(let [a (tokens-to-win %)
                 b (tokens-to-win* %)]
             (when (not= a b)
             {:machine %
              :a a
              :b b})) machines))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 480]
  [:puzzle 29877])


;;;; Part 2

(defn correct-conversion [machine]
  (update machine :target (partial mapv (partial + 10000000000000))))

(defn answer-part-2 [machines]
  (transduce (comp (map correct-conversion) (keep tokens-to-win)) + machines))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle [> 97966918866024] 99423413811305])
