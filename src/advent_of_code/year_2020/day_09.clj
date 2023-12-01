(ns advent-of-code.year-2020.day-09
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(def input (core/get-input))

(defn parse-input [input]
  (map #(Long/parseLong %) (string/split-lines input)))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

(defn answer-part-1 [parsed-input]
  (let [[preamble message] (split-at 25 parsed-input)]
    (loop [[n & message] message
           prior         (into clojure.lang.PersistentQueue/EMPTY preamble)
           pool          (frequencies preamble)]
      (if (some (fn [x] (pool (- n x))) (keys pool))
        (let [leaving (peek prior)]
          (recur message
                 (-> prior pop (conj n))
                 (-> (if (= (pool leaving) 1)
                       (dissoc pool leaving)
                       (update pool leaving dec))
                     (update n (fnil inc 0)))))
        n))))

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 41682220))


;;; Part 2
;;; ============================================================================

(defn answer-part-2 [parsed-input]
  (reduce (fn [[window sum] x]
            (loop [window (conj window x), sum (+ sum x)]
              (cond
                (> sum part-1-answer) (recur (pop window) (- sum (peek window)))
                (= sum part-1-answer) (reduced (+ (apply min window)
                                                  (apply max window)))
                :else                 [window sum])))
          [clojure.lang.PersistentQueue/EMPTY 0]
          parsed-input))

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer 5388976))
