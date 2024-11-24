(ns advent-of-code.year-2020.day-15
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2020 15)


;;;; Parse

(defn parse-input [input] (map parse-long (string/split input #",")))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn make-sequence [start]
  (letfn [(next-number [prev prev-i state]
            (lazy-seq (let [n (- prev-i (state prev prev-i))]
                        (cons n (next-number n
                                             (inc prev-i)
                                             (assoc state prev prev-i))))))]
    (concat start
            (next-number (last start)
                         (dec (count start))
                         (into {} (map vector (butlast start) (range)))))))

(defn memory-game-nth-number [start n]
  (nth (make-sequence start) (dec n)))

(defn answer-part-1 [parsed-input]
  (memory-game-nth-number parsed-input 2020))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 1085])


;;;; Part 2

(defn answer-part-2 [parsed-input]
  (memory-game-nth-number parsed-input 30000000))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 10652])
