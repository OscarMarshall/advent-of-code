(ns advent-of-code-2020.day-24
  (:require [advent-of-code-2020.core :as core]
            [clojure.string :as string]))

;;; Part 1
;;; ============================================================================

(def input (core/get-input))

(defn parse-input [input]
  (map (partial re-seq #"[ns]?[ew]") (string/split-lines input)))

(def parsed-input (parse-input input))

(def direction->vector
  {"e" {:east 1}
   "w" {:east -1}
   "ne" {:north 1, :east 1/2}
   "nw" {:north 1, :east -1/2}
   "se" {:north -1, :east 1/2}
   "sw" {:north -1, :east -1/2}})

(defn day-0-black-tiles [directions]
  (let [tiles (transduce (map (fn [directions]
                                (apply merge-with
                                       +
                                       {:east 0, :north 0}
                                       (map direction->vector directions))))
                         (completing #(update %1 %2 not))
                         {}
                         directions)]
    (into #{} (comp (filter val) (map key)) tiles)))

(defn answer-part-1 [parsed-input]
  (count (day-0-black-tiles parsed-input)))

(def part-1-answer (answer-part-1 parsed-input))

(comment
  part-1-answer
  ;; => 382
  )

;;; Part 2
;;; ============================================================================

(defn neighbors [posn]
  (map (partial merge-with + posn) (vals direction->vector)))

(defn next-day [black-tiles]
  (->> black-tiles
       (reduce (fn [black-neighbors posn]
                 (reduce (fn [black-neighbors posn]
                           (update black-neighbors posn (fnil inc 0)))
                         black-neighbors
                         (neighbors posn)))
               (zipmap black-tiles (repeat 0)))
       (reduce (fn [next-black-tiles [posn black-neighbors]]
                 (cond-> next-black-tiles
                   (if (black-tiles posn)
                     (<= 1 black-neighbors 2)
                     (= black-neighbors 2))
                   (conj posn)))
               #{})))

(defn answer-part-2 [parsed-input]
  (count (nth (iterate next-day (day-0-black-tiles parsed-input)) 100)))

(def part-2-answer (answer-part-2 parsed-input))

(comment
  part-2-answer
  ;; => 3964
  )
