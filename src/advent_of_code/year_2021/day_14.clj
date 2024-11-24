(ns advent-of-code.year-2021.day-14
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2021 14)


;;;; Parse

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

(core/set-parse-fn! parse-input)


;;;; Part 1

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

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 2621])


;;;; Part 2

(defn answer-part-2 [{:keys [pair-insertion-rules polymer-template]}]
  (let [counts (vals (element-counts polymer-template pair-insertion-rules 40))]
    (- (apply max counts) (apply min counts))))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 2843834241366])
