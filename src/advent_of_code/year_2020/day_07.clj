(ns advent-of-code.year-2020.day-07
  (:require [advent-of-code-2020.core :as core]
            [clojure.string :as string]))

(def input (core/get-input))

(defn parse-input [input]
  (into {}
        (map (fn [rule]
               (let [[_ bag contents]
                     (re-matches #"(.*) bags contain (.*)\." rule)]
                 (when (not= contents "no other bags")
                   (let [contents
                         (into {}
                               (map (fn [content]
                                      (let [[_ amount color]
                                            (re-matches #"(\d+) (.*) bags?"
                                                        content)]
                                        [color (Long/parseLong amount)])))
                               (string/split contents #", "))]
                     [bag contents])))))
        (string/split-lines input)))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

(defn answer-part-1 [parsed-input]
  (let [inverted-map (apply merge-with
                            into
                            (map (fn [[bag contents]]
                                   (zipmap (keys contents) (repeat #{bag})))
                                 parsed-input))]
    (loop [[bag & more] (inverted-map "shiny gold"), seen #{}]
      (cond
        (nil? bag) (count seen)
        (seen bag) (recur more seen)
        :else      (recur (concat more (inverted-map bag)) (conj seen bag))))))

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 119))


;;; Part 2
;;; ============================================================================

(def number-of-bags
  (memoize (fn [bag rules]
             (transduce (map (fn [[bag amount]]
                               (* amount (number-of-bags bag rules))))
                        +
                        1
                        (rules bag)))))

(defn answer-part-2 [parsed-input]
  (dec (number-of-bags "shiny gold" parsed-input)))

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer 155802))
