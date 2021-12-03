(ns advent-of-code.day-00
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(def input (core/get-input))

(defn parse-input [input] (string/split-lines input))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

(defn rate [frequencies comparison]
  (-> frequencies
      (->> (map (fn [frequency]
                  (if (comparison (frequency \0) (frequency \1)) \0 \1)))
           (apply str))
      (Long/parseLong 2)))

(defn answer-part-1 [parsed-input]
  (let [frequencies  (map frequencies (apply map vector parsed-input))
        gamma-rate   (rate frequencies >)
        epsilon-rate (rate frequencies <)]
    (* gamma-rate epsilon-rate)))

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 2261546))


;;; Part 2
;;; ============================================================================

(defn rating [numbers comparison]
  (loop [numbers (set numbers), n 0]
    (if (= (count numbers) 1)
      (Long/parseLong (first numbers) 2)
      (let [frequency (frequencies (map #(nth % n) numbers))
            zeros     (frequency \0)
            ones      (frequency \1)]
        (recur (into #{}
                     (filter (comp #{(if (if (= zeros ones)
                                           (comparison 0 1)
                                           (comparison zeros ones))
                                       \0
                                       \1)}
                                   #(nth % n)))
                     numbers)
               (inc n))))))

(defn answer-part-2 [parsed-input]
  (let [oxygen-generator-rating (rating parsed-input >)
        co2-scrubber-rating     (rating parsed-input <)]
    (* oxygen-generator-rating co2-scrubber-rating)))

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer 6775520))
