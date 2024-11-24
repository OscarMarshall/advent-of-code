(ns advent-of-code.year-2021.day-04
  (:require [advent-of-code.core :as core]
            [clojure.set :as set]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2021 4)


;;;; Parse

(defn parse-input [input]
  (let [[[numbers] & boards] (into []
                                   (comp (partition-by #{""}) (remove #{[""]}))
                                   (string/split-lines input))]
    {:numbers (map parse-long (string/split numbers #","))
     :boards  (map (fn [board]
                     (mapv (fn [line]
                             (into []
                                   (comp (remove #{""})
                                         (map parse-long))
                                   (string/split line #" +")))
                           board))
                   boards)}))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn play-board [board numbers]
  (let [board-numbers (into #{} (apply concat board))
        lines         (-> #{}
                          (into (map set board))
                          (into (apply map hash-set board)))]
    (some (fn [[called-numbers just-called]]
            (when (some #(set/subset? % called-numbers) lines)
              [(count called-numbers)
               (* (apply + (set/difference board-numbers called-numbers))
                  just-called)]))
          (map vector (rest (reductions conj #{} numbers)) numbers))))

(defn answer-part-1 [{:keys [numbers boards]}]
  (nth (apply min-key first (map #(play-board % numbers) boards)) 1))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 45031])


;;;; Part 2

(defn answer-part-2 [{:keys [numbers boards]}]
  (nth (apply max-key first (map #(play-board % numbers) boards)) 1))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 2568])
