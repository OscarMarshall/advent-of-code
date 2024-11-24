(ns advent-of-code.year-2021.day-25
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2021 25)


;;;; Parse

(defn parse-input [input]
  (let [lines  (string/split-lines input)
        width  (count (first lines))
        height (count lines)]
    [width height (into {} (for [x     (range width)
                                 y     (range height)
                                 :let  [c (get-in lines [y x])]
                                 :when (not= c \.)]
                             [[x y] c]))]))

(core/set-parse-fn! parse-input)


;;;; Part 1

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

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 498])
