(ns advent-of-code.year-2021.day-07
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2021 7)


;;;; Parse

(defn parse-input [input]
  (map parse-long (string/split (string/trim input) #",")))

(core/set-parse-fn! parse-input)


;;;; Part 1

(def seen-values (atom (sorted-set)))

(defn fuel-needed-fn [fuel-calculation positions]
  (fn [target]
    (swap! seen-values conj target)
    (transduce (map (comp fuel-calculation abs (partial - target)))
               +
               positions)))

(defn optimal-fuel [low high fuel-needed]
  (let [fuel-needed (memoize fuel-needed)]
    (loop [a low, e high]
      (if (< (- e a) 5)
        (transduce (map fuel-needed) min ##Inf (range a (inc e)))
        (let [c (+ a (quot (- e a) 2))
              b (+ a (quot (- c a) 2))
              d (+ c (quot (- e c) 2))]
          (if (< (fuel-needed b) (fuel-needed d))
            (recur a c)
            (recur c e)))))))

(defn answer-part-1 [parsed-input]
  (optimal-fuel (apply min parsed-input)
                (apply max parsed-input)
                (fuel-needed-fn identity parsed-input)))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 337488])


;;;; Part 2

(defn answer-part-2 [parsed-input]
  (optimal-fuel (apply min parsed-input)
                (apply max parsed-input)
                (fuel-needed-fn (fn [x] (/ (* x (+ x 1)) 2)) parsed-input)))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 89647695])
