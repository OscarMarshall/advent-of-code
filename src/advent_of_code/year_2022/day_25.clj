(ns advent-of-code.year-2022.day-25
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]
            [medley.core :as medley]))

(set! *warn-on-reflection* true)

(core/set-date! 2022 25)


;;;; Parse

(defn parse-input [input] (string/split-lines input))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn snafu->decimal [s]
  (apply + (map *
                (replace {\= -2, \- -1, \0 0, \1 1, \2 2} (reverse s))
                (iterate (partial * 5) 1))))

(defn decimal->snafu [decimal]
  (let [ub (medley/find-first (comp #(>= % decimal) snafu->decimal)
                              (map #(repeat % \2) (range)))]
    (-> [[] ub]
        (->> (iterate (fn [[left right]]
                        (let [right (rest right)]
                          [(->> [\2 \1 \0 \- \=]
                                (filter (fn [character]
                                          (-> left
                                              (into (cons character right))
                                              snafu->decimal
                                              (>= decimal))))
                                last
                                (conj left))
                           right]))))
        (nth (count ub))
        first
        (->> (apply str)))))

(defn answer-part-1 [parsed-input]
  (decimal->snafu (transduce (map snafu->decimal) + parsed-input)))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle "2-2--02=1---1200=0-1"])
