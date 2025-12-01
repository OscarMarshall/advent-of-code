(ns advent-of-code.year-2024.day-22
  (:require [advent-of-code.core :as core]
            [clojure.set :as set]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2024 22)


;;;; Parse

(defn parse-input [input] (map parse-long (string/split-lines input)))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn next-secret [secret]
  (let [secret (mod (bit-xor secret (* secret 64)) 16777216)
        secret (mod (bit-xor secret (quot secret 32)) 16777216)
        secret (mod (bit-xor secret (* secret 2048)) 16777216)]
    secret))

(defn answer-part-1 [buyers]
  (transduce (map (fn [secret] (nth (iterate next-secret secret) 2000)))
             +
             buyers))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 37327623]
  [:puzzle 16619522798])


;;;; Part 2

(defn prices-sequence [secrets]
  (map #(mod % 10) secrets))

(defn difference-sequence [prices]
  (map #(- %2 %1) prices (rest prices)))

(defn identify-prices [secrets]
  (let [prices      (prices-sequence secrets)
        differences (difference-sequence prices)]
    (reduce (fn [acc [k v]] (if (acc k) acc (assoc acc k v)))
            {}
            (map vector
                 (map vec (partition 4 1 differences))
                 (drop 4 prices)))))

(defn answer-part-2 [buyers]
  (let [prices-seq (map (comp identify-prices
                               (partial take 2001)
                               (partial iterate next-secret))
                         buyers)]
    (apply max
           (map (fn [k] (transduce (keep #(get % k)) + prices-seq))
                (apply set/union (map (comp set keys) prices-seq))))))

(core/set-answer-fn! 2 answer-part-2
  [:sample2 23]
  [:puzzle 1854])
