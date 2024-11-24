(ns advent-of-code.year-2020.day-24
  (:require [advent-of-code.core :as core]
            [advent-of-code.year-2020.utils :as utils]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2020 24)


;;;; Parse

(defn parse-input [input]
  (map (partial re-seq #"[ns]?[ew]") (string/split-lines input)))

(core/set-parse-fn! parse-input)


;;;; Part 1

(def direction->vector
  {"e" {:east 1}
   "w" {:east -1}
   "ne" {:north 1, :east 1/2}
   "nw" {:north 1, :east -1/2}
   "se" {:north -1, :east 1/2}
   "sw" {:north -1, :east -1/2}})

(defn day-0-black-tiles [directions]
  (let [tiles (transduce (map (fn [directions]
                                (apply merge-with
                                       +
                                       {:east 0, :north 0}
                                       (map direction->vector directions))))
                         (completing #(update %1 %2 not))
                         {}
                         directions)]
    (into #{} (comp (filter val) (map key)) tiles)))

(defn answer-part-1 [parsed-input]
  (count (day-0-black-tiles parsed-input)))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 382])


;;;; Part 2

(defn neighbors [posn]
  (map (partial merge-with + posn) (vals direction->vector)))

(def next-day (utils/conway-step-fn neighbors #(= % 2) #(<= 1 % 2)))

(defn answer-part-2 [parsed-input]
  (count (nth (iterate next-day (day-0-black-tiles parsed-input)) 100)))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 3964])
