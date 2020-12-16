(ns advent-of-code-2020.day-15
  (:require [advent-of-code-2020.core :as core]
            [clojure.string :as string]))

;;; Part 1
;;; ============================================================================

(def input (core/get-input))

(defn parse-input [input]
  (map #(Long/parseLong %) (string/split input #",")))

(def parsed-input (parse-input input))

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

(def part-1-answer (answer-part-1 parsed-input))

(comment
  part-1-answer
  ;; => 1085
  )

;;; Part 2
;;; ============================================================================

(defn answer-part-2 [parsed-input]
  (memory-game-nth-number parsed-input 30000000))

(def part-2-answer (time (answer-part-2 parsed-input)))

(comment
  part-2-answer
  ;; => 10652
  )
