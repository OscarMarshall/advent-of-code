(ns advent-of-code.year-2021.day-25
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(def input (core/get-input *file*))

(defn parse-input [input]
  (let [lines  (string/split-lines input)
        width  (count (first lines))
        height (count lines)]
    [width height (into {} (for [x     (range width)
                                 y     (range height)
                                 :let  [c (get-in lines [y x])]
                                 :when (not= c \.)]
                             [[x y] c]))]))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

(defn step [width height sea-cucumbers]
  (let [sea-cucumbers (reduce (fn [acc [[x y :as posn]]]
                                (let [target [(mod (inc x) width) y]]
                                  (cond-> acc
                                    (nil? (sea-cucumbers target))
                                    (-> (dissoc posn)
                                        (assoc target \>)))))
                              sea-cucumbers
                              (filter (comp #{\>} val) sea-cucumbers))
        sea-cucumbers (reduce (fn [acc [[x y :as posn]]]
                                (let [target [x (mod (inc y) height)]]
                                  (cond-> acc
                                    (nil? (sea-cucumbers target))
                                    (-> (dissoc posn)
                                        (assoc target \v)))))
                              sea-cucumbers
                              (filter (comp #{\v} val) sea-cucumbers))]
    sea-cucumbers))

(defn answer-part-1 [[width height sea-cucumbers]]
  (inc (count (take-while (partial apply not=)
                          (partition 2 1 (iterate (partial step width height)
                                                  sea-cucumbers))))))

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 498))
