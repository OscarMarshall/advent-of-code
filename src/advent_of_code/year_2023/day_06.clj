(ns advent-of-code.year-2023.day-06
  (:require [advent-of-code.core :as core]
            [clojure.math :as math]
            [clojure.string :as str]))

(def input (core/get-input *file*))

(defn parse-input [input]
  (apply map
         (fn [time distance] {:time time, :distance distance})
         (map #(map parse-long (re-seq #"\d+" %)) (str/split-lines input))))

(def parsed-input (parse-input input))


;;;; Part 1

(defn ways-faster [{:keys [time distance]}]
  (let [low (long (math/ceil (/ (- time (math/sqrt (- (math/pow time 2)
                                                      (* 4 distance))))
                                2)))]
    (- time (dec low) low)))

(defn answer-part-1 [races]
  (transduce (map ways-faster) * races))

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 861300))


;;;; Part 2

(defn answer-part-2 [races]
  (let [time     (parse-long (apply str (map :time races)))
        distance (parse-long (apply str (map :distance races)))]
    (ways-faster {:time time, :distance distance})))

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer 28101347))
