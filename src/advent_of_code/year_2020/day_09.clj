(ns advent-of-code.year-2020.day-09
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2020 9)


;;;; Parse

(defn parse-input [input] (map parse-long (string/split-lines input)))

(core/set-parse-fn! parse-input)


;;;; Part 1

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

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 41682220])


;;;; Part 2

(defn answer-part-2 [parsed-input]
  (let [part-1-answer (answer-part-1 parsed-input)]
    (reduce (fn [[window sum] x]
              (loop [window (conj window x), sum (+ sum x)]
                (cond
                  (> sum part-1-answer) (recur (pop window)
                                               (- sum (peek window)))
                  (= sum part-1-answer) (reduced (+ (apply min window)
                                                    (apply max window)))
                  :else                 [window sum])))
            [clojure.lang.PersistentQueue/EMPTY 0]
            parsed-input)))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 5388976])
