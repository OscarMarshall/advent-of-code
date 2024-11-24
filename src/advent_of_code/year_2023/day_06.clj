(ns advent-of-code.year-2023.day-06
  (:require [advent-of-code.core :as core]
            [clojure.math :as math]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)

(core/set-date! 2023 6)


;;;; Parse

(defn parse-input [input]
  (apply map
         (fn [time distance] {:time time, :distance distance})
         (map #(map parse-long (re-seq #"\d+" %)) (str/split-lines input))))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn ways-faster [{:keys [time distance]}]
  (let [low (long (inc (math/floor (/ (- time (math/sqrt (- (math/pow time 2)
                                                            (* 4 distance))))
                                      2))))]
    (- time (dec low) low)))

(defn answer-part-1 [races]
  (transduce (map ways-faster) * races))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 288]
  [:puzzle 861300])


;;;; Part 2

(defn answer-part-2 [races]
  (let [time     (parse-long (apply str (map :time races)))
        distance (parse-long (apply str (map :distance races)))]
    (ways-faster {:time time, :distance distance})))

(core/set-answer-fn! 2 answer-part-2
  [:sample1 71503]
  [:puzzle 28101347])
