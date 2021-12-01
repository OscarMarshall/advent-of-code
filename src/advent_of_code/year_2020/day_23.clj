(ns advent-of-code.year-2020.day-23
  (:require [advent-of-code-2020.core :as core]
            [clojure.string :as string]))

(def input (core/get-input))

(defn parse-input [input]
  (mapv (fn [c] (Long/parseLong (str c))) input))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

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

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer "97632548"))


;;; Part 2
;;; ============================================================================

(defn answer-part-2 [parsed-input]
  (let [cups (into parsed-input (range 10 (inc 1000000)))
        [next-cup] (nth (iterate move (setup-circle cups)) 10000000)]
    (apply * (take 2 (rest (iterate next-cup 1))))))

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer 412990492266))
