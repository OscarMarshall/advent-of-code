(ns advent-of-code.year-2021.day-19
  (:require [advent-of-code.core :as core]
            [clojure.string :as string])
  (:import (clojure.lang PersistentQueue)))

(def input (core/get-input *file*))

(defn parse-input [input]
  (sequence (comp (remove #{""})
                  (partition-by (partial re-matches #"--- scanner \d+ ---"))
                  (remove (comp #{1} count))
                  (map (partial
                        into
                        #{}
                        (map (comp (partial mapv #(Long/parseLong %))
                                   #(string/split % #","))))))
            (string/split-lines input)))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

(defn rotate-posn-x [[x y z]] [x z (- y)])
(defn rotate-posn-y [[x y z]] [(- z) y x])
(defn rotate-posn-z [[x y z]] [y (- x) z])

(defn rotate-beacon-fn [rotate-posn]
  (fn [beacon turns]
    (if (zero? turns) beacon (recur (rotate-posn beacon) (dec turns)))))
(def rotate-beacon-x (rotate-beacon-fn rotate-posn-x))
(def rotate-beacon-y (rotate-beacon-fn rotate-posn-y))
(def rotate-beacon-z (rotate-beacon-fn rotate-posn-z))

(defn rotate-report-fn [rotate-beacon]
  (fn [report turns] (into #{} (map #(rotate-beacon % turns)) report)))
(def rotate-report-x (rotate-report-fn rotate-beacon-x))
(def rotate-report-y (rotate-report-fn rotate-beacon-y))
(def rotate-report-z (rotate-report-fn rotate-beacon-z))

(defn rotate-report [report [x y z]]
  (-> report
      (rotate-report-x x)
      (rotate-report-y y)
      (rotate-report-z z)))

(defn unrotate-report [report [x y z]]
  (-> report
      (rotate-report-z (mod (- z) 4))
      (rotate-report-y (mod (- y) 4))
      (rotate-report-x (mod (- x) 4))))

(def all-rotations
  '([0 0 0]
    [0 0 1]
    [0 0 2]
    [0 0 3]
    [0 1 0]
    [0 1 1]
    [0 1 2]
    [0 1 3]
    [0 2 0]
    [0 2 1]
    [0 3 0]
    [0 3 1]
    [0 3 2]
    [1 0 0]
    [1 0 1]
    [1 0 2]
    [1 0 3]
    [1 2 0]
    [1 2 1]
    [1 3 0]
    [2 0 0]
    [2 0 1]
    [3 0 0]
    [3 0 1]))

(defn move-report [report amount]
  (into #{} (map (partial mapv + amount)) report))

(defn unmove-report [report amount]
  (into #{} (map #(mapv - % amount)) report))

(defn normalize-report [report]
  (->> all-rotations
       (map (fn [rotate-amount]
              (let [report      (rotate-report report rotate-amount)
                    min-coords  (reduce (partial map min) report)
                    move-amount (map - min-coords)]
                [(move-report report move-amount)
                 {:rotate-amount rotate-amount, :move-amount move-amount}])))
       (sort-by (comp vec sort first))
       first))

(def group-beacons
  (memoize
   (fn [beacons]
     (let [beacons (sort-by #(nth % 0) beacons)]
       (for [beacons (concat (take-while (comp (partial <= 12) count) ; -x
                                         (iterate rest beacons))
                             (take-while (comp (partial <= 12) count) ; +x
                                         (iterate rest (reverse beacons))))
             :let    [beacons (sort-by #(nth % 1) beacons)]
             beacons (concat (take-while (comp (partial <= 12) count) ; -y
                                         (iterate rest beacons))
                             (take-while (comp (partial <= 12) count) ; +y
                                         (iterate rest (reverse beacons))))
             :let    [beacons (sort-by #(nth % 2) beacons)]
             beacons [(into #{} (take 12) beacons)
                      (into #{} (take 12) (reverse beacons))]]
         (normalize-report beacons))))))

(def combine-reports
  (memoize
   (fn [[report0 & reports]]
     (loop [reports  (into PersistentQueue/EMPTY reports)
            groups   (into {} (group-beacons report0))
            scanners #{[0 0 0]}
            beacons  report0]
       (println (count reports))
       (if (empty? reports)
         {:scanners scanners, :beacons beacons}
         (let [report  (peek reports)
               reports (pop reports)]
           (if-some [[new-scanner new-beacons]
                     (some
                      (fn [[group
                            {:keys [rotate-amount move-amount]}]]
                        (when-some [{rotate-amount* :rotate-amount
                                     move-amount*   :move-amount}
                                    (groups group)]
                          [(-> #{[0 0 0]}
                               (move-report move-amount)
                               (unmove-report move-amount*)
                               (unrotate-report rotate-amount*)
                               first)
                           (-> report
                               (rotate-report rotate-amount)
                               (move-report move-amount)
                               (unmove-report move-amount*)
                               (unrotate-report rotate-amount*))]))
                      (group-beacons report))]
             (recur reports
                    (into groups (group-beacons new-beacons))
                    (conj scanners new-scanner)
                    (into beacons new-beacons))
             (recur (conj reports report)
                    groups
                    scanners
                    beacons))))))))

(defn answer-part-1 [parsed-input]
  (count (:beacons (combine-reports parsed-input))))

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 372))


;;; Part 2
;;; ============================================================================

(defn answer-part-2 [parsed-input]
  (let [{:keys [scanners]} (combine-reports parsed-input)]
    (reduce max
            (for [[a & bs] (take-while some? (iterate next scanners))
                  b        bs]
              (apply + (map (comp #(Math/abs %) -) a b))))))

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer 12241))
