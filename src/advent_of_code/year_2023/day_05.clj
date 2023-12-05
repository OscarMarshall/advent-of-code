(ns advent-of-code.year-2023.day-05
  (:require [advent-of-code.core :as core]
            [clojure.string :as str]))

(def input (core/get-input))

(defn parse-conversion-map [s]
  (map #(mapv parse-long (str/split % #" ")) (rest (str/split-lines s))))

(defn parse-input [input]
  (let [[seeds-str & map-strs] (str/split input #"\n\n")
        seeds                  (map parse-long (re-seq #"\d+" seeds-str))]
    {:seeds           seeds
     :conversion-maps (map parse-conversion-map map-strs)}))

(def parsed-input (parse-input input))


;;;; Part 1

(defn convert [id conversion-map]
  (or (some (fn [[destination-start source-start length]]
              (when (<= source-start id (dec (+ source-start length)))
                (+ destination-start (- id source-start))))
            conversion-map)
      id))

(defn answer-part-1 [{:keys [seeds conversion-maps]}]
  (transduce (map #(reduce convert % conversion-maps)) min ##Inf seeds))

(def part-1-answer (answer-part-1 parsed-input))

(assert (not= part-1-answer 2036236))
(assert (= part-1-answer 177942185))


;;;; Part 2

(defn answer-part-2 [x]
  x)

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer part-2-answer))
