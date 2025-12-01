(ns advent-of-code.year-2024.day-07
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]
            [medley.core :as medley]))

(set! *warn-on-reflection* true)

(core/set-date! 2024 7)


;;;; Parse

(defn parse-input [input]
  (map (fn [line]
         (let [[test numbers] (string/split line #": ")]
           {:test    (parse-long test)
            :numbers (mapv parse-long (string/split numbers #" "))}))
       (string/split-lines input)))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn possibly-true? [test numbers operators]
  (if-some [number (peek numbers)]
    (let [numbers (pop numbers)]
      (and (<= number test)
           (some (fn [f]
                   (some-> test
                           (f number)
                           (possibly-true? numbers operators)))
                 operators)))
    (zero? test)))

(defn answer-part-1 [calibrations]
  (->> calibrations
       (filter (fn [{:keys [test numbers]}]
                 (possibly-true? test numbers [-
                                               (fn [x y]
                                                 (when (zero? (rem x y))
                                                   (quot x y)))])))
       (map :test)
       (apply +)))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 3749]
  [:puzzle 12553187650171])


;;;; Part 2

(def tens (iterate (partial * 10) 1))

(defn answer-part-2 [calibrations]
  (->> calibrations
       (filter (fn [{:keys [test numbers]}]
                 (possibly-true? test
                                 numbers
                                 [-
                                  (fn [x y]
                                    (when (zero? (rem x y))
                                      (quot x y)))
                                  (fn [x y]
                                    (let [magnitude (medley/find-first #(> % y)
                                                                       tens)]
                                      (when (= (rem x magnitude) y)
                                        (quot x magnitude))))])))
       (map :test)
       (apply +)))

(core/set-answer-fn! 2 answer-part-2
  [:sample1 11387]
  [:puzzle 96779702119491])
