(ns advent-of-code.year-2020.day-23
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2020 23)


;;;; Parse

(defn parse-input [input] (mapv (comp parse-long str) input))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn setup-circle [cups]
  [(into {} (map vector cups (rest (cycle cups)))) (first cups)])

(defn move [[next-cup current]]
  (let [[a b c d]   (rest (iterate next-cup current))
        destination (loop [x (dec current)]
                      (cond
                        (zero? x)    (recur (count next-cup))
                        (#{a b c} x) (recur (dec x))
                        :else        x))]
    [(assoc next-cup current d, destination a, c (next-cup destination))
     d]))

(defn answer-string [[next-cup]]
  (string/join (take-while (complement #{1}) (rest (iterate next-cup 1)))))

(defn answer-part-1 [parsed-input]
  (answer-string (nth (iterate move (setup-circle parsed-input)) 100)))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle "97632548"])


;;;; Part 2

(defn answer-part-2 [parsed-input]
  (let [cups (into parsed-input (range 10 (inc 1000000)))
        [next-cup] (nth (iterate move (setup-circle cups)) 10000000)]
    (apply * (take 2 (rest (iterate next-cup 1))))))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 412990492266])
