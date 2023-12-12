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

(def possible-arrangements
  (memoize
   (fn [springs damaged-groups]
     (if (= (count springs)
            (+ (apply + damaged-groups) (dec (count damaged-groups))))
       (if (every? identity
                   (map #(%1 %2)
                        (flatten (interpose [#{\. \?}]
                                            (map #(repeat % #{\# \?})
                                                 damaged-groups)))
                        springs))
         1
         0)
       (case (first springs)
         nil (if (empty? damaged-groups) 1 0)
         \.  (recur (rest springs) damaged-groups)
         \#  (if-some [damaged-group (first damaged-groups)]
               (if (and (every? #{\# \?} (take damaged-group springs))
                        (#{\. \?} (nth springs damaged-group nil)))
                 (recur (drop (inc damaged-group) springs)
                        (rest damaged-groups))
                 0)
               0)
         \?  (let [springs-tail (rest springs)]
               (transduce (map #(possible-arrangements (cons % springs-tail)
                                                       damaged-groups))
                          +
                          [\. \#])))))))

(defn answer-part-1 [condition-records]
  (transduce (map #(apply possible-arrangements %))
             +
             condition-records))

(core/part 1
  parse-input answer-part-1 *file*
  [:sample1 21]
  [:input 7195])


;;;; Part 2

(defn unfold-condition-record [[springs damaged-groups]]
  [(str/join "?" (repeat 5 springs)) (flatten (repeat 5 damaged-groups))])

(defn answer-part-2 [condition-records]
  (answer-part-1 (map unfold-condition-record condition-records)))

(core/part 2
  parse-input answer-part-2 *file*
  [:sample1 525152]
  [:input 33992866292225])
