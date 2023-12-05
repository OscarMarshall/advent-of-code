(ns advent-of-code.year-2023.day-05
  (:require [advent-of-code.core :as core]
            [clojure.string :as str]))

(def input (core/get-input))

(defn parse-conversion-map [s]
  (sort (map #(mapv parse-long (str/split % #" ")) (rest (str/split-lines s)))))

(defn parse-input [input]
  (let [[seeds-str & map-strs] (str/split input #"\n\n")
        seeds                  (map parse-long (re-seq #"\d+" seeds-str))]
    {:seeds           seeds
     :conversion-maps (map parse-conversion-map map-strs)}))

(def parsed-input (parse-input input))


;;;; Part 1

(defn convert [id conversion-map]
  (or (when-some [[destination-start source-start length]
                  (last (take-while #(<= (second %) id) conversion-map))]
        (when (< id (+ source-start length))
          (+ destination-start (- id source-start))))
      id))

(defn answer-part-1 [{:keys [seeds conversion-maps]}]
  (transduce (map #(reduce convert % conversion-maps)) min ##Inf (rest seeds)))

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 2036236))


;;;; Part 2

(defn answer-part-2 [x]
  x)

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer part-2-answer))
