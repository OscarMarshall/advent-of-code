(ns advent-of-code.year-2023.day-13
  (:require [advent-of-code.core :as core]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)

(core/set-date! 2023 13)


;;;; Parse

(defn parse-input [input] (map str/split-lines (str/split input #"\n\n")))

(core/set-parse-fn! parse-input)

;;;; Part 1

(defn reflect [pattern] (apply map list pattern))

(defn find-mirror-value [pattern]
  (some (fn [[pre post]]
          (when (every? true? (map = (reverse pre) post)) (count pre)))
        (map #(split-at % pattern) (range 1 (count pattern)))))

(defn answer [patterns find-fn]
  (transduce (map #(or (find-fn (reflect %)) (* (find-fn %) 100))) + patterns))

(defn answer-part-1 [patterns]
  (answer patterns find-mirror-value))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 405]
  [:puzzle 34821])


;;;; Part 2

(defn find-smudged-mirror-value [pattern]
  (some (fn [[pre post]]
          (when (= (count (filter false? (map =
                                              (apply concat (reverse pre))
                                              (apply concat post))))
                   1)
            (count pre)))
        (map #(split-at % pattern) (range 1 (count pattern)))))

(defn answer-part-2 [patterns] (answer patterns find-smudged-mirror-value))

(core/set-answer-fn! 2 answer-part-2
  [:sample1 400]
  [:puzzle 36919])
