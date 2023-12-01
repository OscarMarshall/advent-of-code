(ns advent-of-code.year-2021.day-10
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(def input (core/get-input))

(defn parse-input [input] (string/split-lines input))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

(def open->close {\( \), \[ \], \{ \}, \< \>})

(def open-chars (set (keys open->close)))

(def syntax-error-score {\) 3, \] 57, \} 1197, \> 25137})

(defn process-line [line]
  (loop [[char & line] (seq line), expected-closings ()]
    (cond
      (nil? char)                       expected-closings
      (open-chars char)                 (recur line (conj expected-closings
                                                          (open->close char)))
      (= char (peek expected-closings)) (recur line (pop expected-closings))
      :else                             char)))

(defn answer-part-1 [parsed-input]
  (transduce (comp (map process-line) (filter char?) (map syntax-error-score))
             +
             parsed-input))

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 319233))


;;; Part 2
;;; ============================================================================

(def close-points {\) 1, \] 2, \} 3, \> 4})

(defn autocomplete-score [closings]
  (transduce (map close-points)
             (completing (fn [acc x] (+ (* acc 5) x)))
             0
             closings))

(defn answer-part-2 [parsed-input]
  (let [processed-lines (sort (eduction (map process-line)
                                        (filter list?)
                                        (map autocomplete-score)
                                        parsed-input))]
    (nth processed-lines (quot (count processed-lines) 2))))

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer 1118976874))
