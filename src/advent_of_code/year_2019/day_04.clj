(ns advent-of-code.year-2019.day-04
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2019 4)


;;;; Parse

(defn parse-input [input]
  (map parse-long (string/split (string/trim input) #"-")))


(core/set-parse-fn! parse-input)

;;;; Part 1

(defn password->digits [password]
  (some (fn [[acc password]] (and (zero? password) acc))
        (iterate (fn [[acc password]]
                   [(cons (rem password 10) acc) (quot password 10)])
                 [() password])))

(defn has-double? [digits]
  (and (> (count digits) 1)
       (or (= (first digits) (second digits)) (recur (rest digits)))))

(defn increases? [digits]
  (or (< (count digits) 2)
      (and (<= (first digits) (second digits)) (recur (rest digits)))))

(defn answer-part-1 [[minimum maximum]]
  (count (filter (comp (every-pred has-double? increases?)
                       password->digits)
                 (range minimum (inc maximum)))))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 1694])


;;;; Part 2

(defn has-strict-double? [[first-digit & digits]]
  (and (seq digits) (let [[repeats digits] (split-with #{first-digit} digits)]
                      (or (= (count repeats) 1) (recur digits)))))

(defn answer-part-2 [[minimum maximum]]
  (count (filter (comp (every-pred has-strict-double? increases?)
                       password->digits)
                 (range minimum (inc maximum)))))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 1148])
