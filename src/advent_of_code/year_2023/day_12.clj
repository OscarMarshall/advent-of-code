(ns advent-of-code.year-2023.day-12
  (:require [advent-of-code.core :as core]
            [clojure.string :as str]))

(println "# Day 12")

(set! *warn-on-reflection* true)

(defn parse-input [input]
  (map (fn [s] (let [[springs damaged-groups] (str/split s #" " 2)]
                 [springs (map parse-long (str/split damaged-groups #","))]))
       (str/split-lines input)))

;;;; Part 1

(defn possible-arrangements [springs damaged-groups]
  (if (and (= (count damaged-groups) 1)
           (= (first damaged-groups) (count springs)))
    (when (every? #{\# \?} springs) [(repeat (first damaged-groups) \#)])
    (case (first springs)
      nil (when (empty? damaged-groups) [[]])
      \.  (map #(cons \. %)
               (possible-arrangements (rest springs) damaged-groups))
      \#  (when-some [damaged-group (first damaged-groups)]
            (when (and (every? #{\# \?} (take damaged-group springs))
                       (#{\. \?} (nth springs damaged-group nil)))
              (let [lead-springs (concat (repeat damaged-group \#) [\.])]
                (map #(concat lead-springs %)
                     (possible-arrangements (drop (inc damaged-group) springs)
                                            (rest damaged-groups))))))
      \?  (mapcat #(possible-arrangements (cons % (rest springs))
                                          damaged-groups)
                  [\. \#]))))

(defn answer-part-1 [condition-records]
  (transduce (map #(count (apply possible-arrangements %)))
             +
             condition-records))

(core/part 1
  parse-input answer-part-1 *file*
  [:sample1 21]
  [:input 7195])


;;;; Part 2

(defn answer-part-2 [x]
  x)

(core/part 2
  parse-input answer-part-2 *file*
  #_[:sample1]
  [:input])
