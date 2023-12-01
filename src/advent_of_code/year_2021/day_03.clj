(ns advent-of-code.year-2021.day-03
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(def input (core/get-input))

(defn parse-input [input] (string/split-lines input))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

(defn rate [frequencies comparison]
  (Long/parseLong (apply str (map (fn [{zeros \0, ones \1}]
                                    (if (comparison zeros ones) \0 \1))
                                  frequencies))
                  2))

(defn answer-part-1 [parsed-input]
  (let [frequencies  (map frequencies (apply map vector parsed-input))]
    (* (rate frequencies >) (rate frequencies <))))

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 2261546))


;;; Part 2
;;; ============================================================================

(defn rating [numbers comparison]
  (reduce (fn [numbers n]
            (if (= (count numbers) 1)
              (reduced (Long/parseLong (first numbers) 2))
              (let [{zeros \0 ones \1} (frequencies (map #(nth % n) numbers))]
                (into #{}
                      (filter (fn [x]
                                (#{(if (if (= zeros ones)
                                         (comparison 0 1)
                                         (comparison zeros ones))
                                     \0
                                     \1)}
                                 (nth x n))))
                      numbers))))
          (set numbers)
          (range (count numbers))))

(defn answer-part-2 [parsed-input]
  (* (rating parsed-input >) (rating parsed-input <)))

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer 6775520))
