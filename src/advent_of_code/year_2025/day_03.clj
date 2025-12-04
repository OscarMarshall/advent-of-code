(ns advent-of-code.year-2025.day-03
  (:require
   [advent-of-code.core :as core]
   [clojure.string :as string]
   [medley.core :as medley]))

(set! *warn-on-reflection* true)

(core/set-date! 2025 3)


;;;; Parse

(defn parse-input [input]
  (map (partial map (comp parse-long str)) (string/split-lines input)))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn largest-joltage [bank n]
  (let [window-size (- (count bank) n)]
    (first (reduce (fn [[result window] z]
                     (let [window (concat window [z]), a (apply max window)]
                       [(+ (* result 10) a) (medley/drop-upto #{a} window)]))
                   [0 (take window-size bank)]
                   (drop window-size bank)))))

(defn largest-joltage* [bank n]
  (let [bank (apply str bank)
        window-size (- (count bank) n)]
    (->> (range (inc window-size) ##Inf)
         (take n)
         (reduce (fn [[result start] end]
                   (let [window (subs bank start end)
                         a (apply medley/greatest window)]
                     [(conj result a)
                      (inc (string/index-of bank (str a) start))]))
                 [[] 0])
         first
         (apply str)
         parse-long)))

(defn answer-part-1 [banks] (transduce (map #(largest-joltage % 2)) + banks))

(core/set-answer-fn! 1 answer-part-1 [:sample1 357] [:puzzle 17281])


;;;; Part 2

(defn answer-part-2 [banks] (transduce (map #(largest-joltage % 12)) + banks))

(core/set-answer-fn! 2 answer-part-2
  [:sample1 3121910778619]
  [:puzzle 171388730430281])
