(ns advent-of-code.year-2023.day-25
  (:require [advent-of-code.core :as core]
            [clojure.set :as set]
            [clojure.string :as string]
            [medley.core :as medley]))

(set! *warn-on-reflection* true)

(core/set-date! 2023 25)


;;;; Parse

(defn parse-input [input]
  (transduce (comp (map #(string/split % #": "))
                   (map (fn [[source sinks]]
                          [source (string/split sinks #" ")])))
             (completing (fn [result [source sinks]]
                           (reduce (fn [result sink]
                                     (-> result
                                         (update source (fnil conj #{}) sink)
                                         (update sink (fnil conj #{}) source)))
                                   result
                                   sinks)))
             {}
             (string/split-lines input)))

(core/set-parse-fn! parse-input)

;;;; Part 1

(defn answer-part-1 [graph]
  (let [graph-size (count graph)]
    (loop [a (set (keys graph))]
      (let [external-neighbors (into {}
                                     (map (fn [component]
                                            [component (-> component
                                                           graph
                                                           (set/difference a)
                                                           count)]))
                                     a)]
        (if (= (transduce (map val) + external-neighbors) 3)
          (let [a-size (count a)]
            (* a-size (- graph-size a-size)))
          (recur (disj a (apply medley/greatest-by external-neighbors a))))))))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 54]
  [:puzzle 600225])
