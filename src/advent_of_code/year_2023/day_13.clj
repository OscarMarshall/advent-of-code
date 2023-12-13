(ns advent-of-code.year-2023.day-13
  (:require [advent-of-code.core :as core]
            [clojure.string :as str]))

(println "# Day 13")

(set! *warn-on-reflection* true)

(defn parse-input [input] (map str/split-lines (str/split input #"\n\n")))

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

(core/part 1
  parse-input answer-part-1 *file*
  [:sample1 405]
  [:input 34821])


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

(core/part 2
  parse-input answer-part-2 *file*
  [:sample1 400]
  [:input 36919])
