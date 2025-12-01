(ns advent-of-code.year-2024.day-11
  (:require [advent-of-code.core :as core]
            [clojure.math :as math]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2024 11)


;;;; Parse

(defn parse-input [input]
  (map parse-long (string/split (string/trim input) #" ")))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn blink [stone]
  (if (zero? stone)
    [1]
    (let [length (inc (long (math/log10 stone)))]
      (if (even? length)
        (let [magnitude (long (math/pow 10 (quot length 2)))]
          [(quot stone magnitude) (rem stone magnitude)])
        [(* stone 2024)]))))

(def count-after-blinks
  (memoize
   (fn [stone blinks]
     (if (zero? blinks)
       1
       (let [blinks (dec blinks)]
         (transduce (map #(count-after-blinks % blinks)) + (blink stone)))))))

(defn answer-part-1 [stones]
  (transduce (map #(count-after-blinks % 25)) + stones))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 55312]
  [:puzzle 187738])


;;;; Part 2

(defn answer-part-2 [stones]
  (transduce (map #(count-after-blinks % 75)) + stones))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 223767210249237])
