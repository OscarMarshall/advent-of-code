(ns advent-of-code.year-2023.day-21
  (:require [advent-of-code.core :as core]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]
            [medley.core :as medley]))

(println "# Day 21")

(set! *warn-on-reflection* true)

(defn parse-input [input] (mapv vec (str/split-lines input)))

;;;; Part 1

(def garden-plots #{\. \S})

(defn all-locations [chart]
  (map vec (combo/cartesian-product (range (count chart))
                                    (range (count (first chart))))))

(defn starting-location [chart]
  (medley/find-first #(= (get-in chart %) \S) (all-locations chart)))

(def neighbor-vectors [[-1 0] [0 1] [1 0] [0 -1]])

(defn possible-steps [chart location]
  (eduction (map #(mapv + location %))
            (filter #(garden-plots (get-in chart %)))
            neighbor-vectors))

(defn possible-locations [chart start]
  (iterate (fn [locations]
             (into #{}
                   (mapcat #(possible-steps chart %))
                   locations))
           #{start}))

(defn answer-part-1 [chart]
  (count (nth (possible-locations chart (starting-location chart)) 64)))

(core/part 1
  parse-input answer-part-1 *file*
  [:input 3768])


;;;; Part 2

(defn print-possible-locations [chart locations]
  (println (str/join "\n"
                     (map #(apply str %)
                          (reduce #(assoc-in %1 %2 \O) chart locations)))))

(defn answer-part-2 [chart]
  (let [even-edge-length (quot 26501365 (count chart))
        odd-edge-length  (dec even-edge-length)
        locations-seq    (drop 130 (possible-locations chart [65 65]))
        even-locations   (first locations-seq)
        odd-locations    (second locations-seq)]
    (+ (apply + (map (fn [starting-location]
                       (let [locations-seq
                             (drop 64
                                   (possible-locations chart starting-location))

                             small-edge (first locations-seq)
                             large-edge (nth locations-seq 131)]
                         (+ (* even-edge-length (count small-edge))
                            (* odd-edge-length (count large-edge)))))
                     (combo/cartesian-product [0 130] [0 130])))
       (apply + (map (fn [starting-location]
                       (count (nth (possible-locations chart starting-location)
                                   130)))
                     [[130 65] [65 0] [0 65] [65 130]]))
       (* odd-edge-length odd-edge-length (count odd-locations))
       (* even-edge-length even-edge-length (count even-locations)))))

(core/part 2
  parse-input answer-part-2 *file*
  [:input
   [> 314631336880811]
   [< 629258745301963]
   [> 627955357120811]
   [not= 627955370067979]
   [not= 627961524847254]
   [not= 627960747389133]
   [not= 627960775932622]
   627960775905777])
