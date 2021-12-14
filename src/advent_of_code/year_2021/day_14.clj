(ns advent-of-code.year-2021.day-14
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(def input (core/get-input))

(defn parse-input [input]
  (let [[[polymer-template] _ pair-insertion-rules]
        (partition-by #{""} (string/split-lines input))]
    {:polymer-template     (seq polymer-template)
     :pair-insertion-rules (into {}
                                 (map (fn [line]
                                        (let [[_ [x] [y] [z]] (re-matches
                                                               #"(.)(.) -> (.)"
                                                               line)]
                                          [[x y] z])))
                                 pair-insertion-rules)}))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

;;; NB: This doesn't count the right element in the pair.
(def element-counts*
  (memoize
   (fn [[left right :as pair] pair-insertion-rules steps]
     (if (zero? steps)
       {left 1}
       (let [inserted-element (pair-insertion-rules pair)
             steps            (dec steps)]
         (merge-with +
                     (element-counts* [left inserted-element]
                                      pair-insertion-rules
                                      steps)
                     (element-counts* [inserted-element right]
                                      pair-insertion-rules
                                      steps)))))))

(defn element-counts [polymer-template pair-insertion-rules steps]
  (apply merge-with
         +
         {(last polymer-template) 1}
         (map #(element-counts* % pair-insertion-rules steps)
              (partition 2 1 polymer-template))))

(defn answer-part-1 [{:keys [pair-insertion-rules polymer-template]}]
  (let [counts (vals (element-counts polymer-template pair-insertion-rules 10))]
    (- (apply max counts) (apply min counts))))

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 2621))


;;; Part 2
;;; ============================================================================

(defn answer-part-2 [{:keys [pair-insertion-rules polymer-template]}]
  (let [counts (vals (element-counts polymer-template pair-insertion-rules 40))]
    (- (apply max counts) (apply min counts))))

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer 2843834241366))
