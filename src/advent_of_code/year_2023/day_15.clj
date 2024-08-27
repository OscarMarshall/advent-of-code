(ns advent-of-code.year-2023.day-15
  (:require [advent-of-code.core :as core]
            [clojure.string :as str]
            [medley.core :as medley])
  (:import (java.util List)))

(println "# Day 15")

(set! *warn-on-reflection* true)

(defn parse-input [input] (str/split (str/replace input "\n" "") #","))

;;;; Part 1

(defn hash-algorithm [s]
  (transduce (map long)
             (completing (fn [current-value ascii-code]
                           (mod (* (+ current-value ascii-code) 17) 256)))
             0
             s))

(defn answer-part-1 [instructions]
  (transduce (map hash-algorithm)
             +
             instructions))

(core/part 1
  parse-input answer-part-1 *file*
  [:sample1 1320]
  [:input [not= 517847] 517965])


;;;; Part 2

(defn perform-step [boxes s]
  (condp re-matches s
    #"(.*)-"      :>> (fn [[_ label]]
                        (update
                         boxes
                         (hash-algorithm label)
                         (fnil (fn [[^List order focal-lengths :as box]]
                                 (let [index (.indexOf order label)]
                                   (if (not= index -1)
                                     [(vec (medley/remove-nth index order))
                                      focal-lengths]
                                     box)))
                               [[] {}])))
    #"(.*)=(\d+)" :>> (fn [[_ label focal-length]]
                        (update boxes
                                (hash-algorithm label)
                                (fnil (fn [[^List order focal-lengths]]
                                        (let [index (.indexOf order label)]
                                          [(cond-> order
                                             (= index -1) (conj label))
                                           (assoc focal-lengths
                                                  label
                                                  (parse-long focal-length))]))
                                      [[] {}])))))

(defn calculate-total-focusing-power [boxes]
  (apply + (for [[box [order focal-lengths]] boxes
                 [index label]               (map-indexed vector order)]
             (* (inc box) (inc index) (focal-lengths label)))))

(defn answer-part-2 [instructions]
  (calculate-total-focusing-power (reduce perform-step {} instructions)))

(core/part 2
  parse-input answer-part-2 *file*
  [:sample1 145]
  [:input 267372])
