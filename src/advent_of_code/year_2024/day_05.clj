(ns advent-of-code.year-2024.day-05
  (:require [advent-of-code.core :as core]
            [clojure.set :as set]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2024 5)


;;;; Parse

(defn parse-input [input]
  (let [[rules updates] (string/split input #"\n\n")]
    {:rules   (map (fn [rule]
                     (map parse-long (string/split rule #"\|")))
                   (string/split-lines rules))
     :updates (map (fn [update]
                     (mapv parse-long (string/split update #",")))
                   (string/split-lines updates))}))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn rules->successors [rules]
  (update-vals (group-by first rules) (partial into #{} (map second))))

(defn correct-update? [update successors]
  (not-any? seq (map (fn [seen page] (set/intersection (successors page) seen))
                     (reductions conj #{} update)
                     update)))

(defn middle-page [update]
  (nth update (quot (count update) 2)))

(defn answer-part-1 [{:keys [rules updates]}]
  (let [successors (rules->successors rules)]
    (transduce (comp (filter #(correct-update? % successors)) (map middle-page))
               +
               updates)))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 143]
  [:puzzle 4689])


;;;; Part 2

(defn answer-part-2 [{:keys [rules updates]}]
  (let [successors (rules->successors rules)]
    (transduce (comp (remove #(correct-update? % successors))
                     (map (comp middle-page
                                (partial sort (fn [x y]
                                                (if ((successors x #{}) y)
                                                  -1
                                                  1))))))
               +
               updates)))

(core/set-answer-fn! 2 answer-part-2
  [:sample1 123]
  [:puzzle 6336])
