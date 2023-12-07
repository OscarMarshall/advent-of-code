(ns advent-of-code.year-2021.day-04
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]
            [clojure.set :as set]))

(def input (core/get-input *file*))

(defn parse-input [input]
  (let [[[numbers] & boards] (into []
                                   (comp (partition-by #{""}) (remove #{[""]}))
                                   (string/split-lines input))]
    {:numbers (map #(Long/parseLong %) (string/split numbers #","))
     :boards  (map (fn [board]
                     (mapv (fn [line]
                             (into []
                                   (comp (remove #{""})
                                         (map #(Long/parseLong %)))
                                   (string/split line #" +")))
                           board))
                   boards)}))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

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

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 45031))


;;; Part 2
;;; ============================================================================

(defn answer-part-2 [{:keys [numbers boards]}]
  (nth (apply max-key first (map #(play-board % numbers) boards)) 1))

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer 2568))
