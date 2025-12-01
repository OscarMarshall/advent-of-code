(ns advent-of-code.year-2024.day-19
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2024 19)


;;;; Parse

(defn parse-input [input]
  (let [[towels designs] (string/split input #"\n\n")]
    {:towels   (string/split towels #", ")
     :designs (string/split-lines designs)}))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn possible-design? [letter->towels design]
  (or (empty? design)
      (some (fn [towel]
              (and (string/starts-with? design towel)
                   (possible-design? letter->towels
                                     (subs design (count towel)))))
            (letter->towels (first design)))))

(defn answer-part-1 [{:keys [towels designs]}]
  (let [letter->towels (group-by first towels)]
    (count (filter (partial possible-design? letter->towels) designs))))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 6]
  [:puzzle 228])


;;;; Part 2

(def design-ways
  (memoize
   (fn [letter->towels design]
     (if (empty? design)
       1
       (transduce (keep (fn [towel]
                          (when (string/starts-with? design towel)
                            (design-ways letter->towels
                                         (subs design (count towel))))))
                  +
                  (letter->towels (first design)))))))

(defn answer-part-2 [{:keys [towels designs]}]
  (let [letter->towels (group-by first towels)]
    (transduce (map (partial design-ways letter->towels)) + designs)))

(core/set-answer-fn! 2 answer-part-2
  [:sample1 16]
  [:puzzle 584553405070389])
