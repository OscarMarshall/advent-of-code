(ns advent-of-code.year-2023.day-05
  (:require [advent-of-code.core :as core]
            [clojure.string :as str]
            [medley.core :as medley]))

(set! *warn-on-reflection* true)

(core/set-date! 2023 5)


;;;; Parse

(defn parse-conversion-map [s]
  (map #(mapv parse-long (str/split % #" ")) (rest (str/split-lines s))))

(defn parse-input [input]
  (let [[seeds-str & map-strs] (str/split input #"\n\n")
        seeds                  (map parse-long (re-seq #"\d+" seeds-str))]
    {:seeds           seeds
     :conversion-maps (map parse-conversion-map map-strs)}))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn convert [id conversion-map]
  (or (some (fn [[destination-start source-start length]]
              (when (<= source-start id (dec (+ source-start length)))
                (+ destination-start (- id source-start))))
            conversion-map)
      id))

(defn answer-part-1 [{:keys [seeds conversion-maps]}]
  (transduce (map #(reduce convert % conversion-maps)) min ##Inf seeds))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 35]
  [:puzzle [> 2036236] 177942185])


;;;; Part 2

(defn convert-range [[id-start id-length :as id-range] conversion-map]
  (or (let [id-end (+ id-start id-length)]
        (some (fn [[destination-start source-start length]]
                (let [source-end (+ source-start length)]
                  (cond
                    ;; Contained
                    (<= source-start id-start (dec id-end) (dec source-end))
                    [[(+ destination-start (- id-start source-start))
                      id-length]]

                    ;; Beginning overlaps
                    (<= source-start id-start (dec source-end) (dec id-end))
                    (mapcat #(convert-range % conversion-map)
                            [[id-start (- source-end id-start)]
                             [source-end (- id-end source-end)]])

                    ;; End overlaps
                    (< id-start source-start id-end)
                    (mapcat #(convert-range % conversion-map)
                            [[id-start (- source-start id-start)]
                             [source-start (- id-end source-start)]]))))
              conversion-map))
      [id-range]))

(defn answer-part-2 [{:keys [seeds conversion-maps]}]
  (->> conversion-maps
       (reduce (fn [ranges conversion-map]
                 (mapcat #(convert-range % conversion-map) ranges))
               (partition 2 seeds))
       (apply medley/least)
       first))

(core/set-answer-fn! 2 answer-part-2
  [:sample1 46]
  [:puzzle [> 59390325 69841803]])
