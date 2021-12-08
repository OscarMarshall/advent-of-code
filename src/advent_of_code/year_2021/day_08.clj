(ns advent-of-code.year-2021.day-08
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]
            [clojure.set :as set])
  (:import (clojure.lang PersistentQueue)))

(def input (core/get-input))

(defn parse-signal-patterns [signal-patterns]
  (map (partial into #{}) (string/split signal-patterns #" ")))

(defn parse-input [input]
  (map (fn [s]
         (let [[examples output] (map parse-signal-patterns
                                      (string/split s #" \| "))]
           {:examples examples, :output output}))
       (string/split-lines input)))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

(defn answer-part-1 [parsed-input]
  (count (into ()
               (comp (mapcat :output) (map count) (filter #{2 3 4 7}))
               parsed-input)))

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 342))


;;; Part 2
;;; ============================================================================

(def segments->number
  {#{\a \b \c \e \f \g}    0
   #{\c \f}                1
   #{\a \c \d \e \g}       2
   #{\a \c \d \f \g}       3
   #{\b \c \d \f}          4
   #{\a \b \d \f \g}       5
   #{\a \b \d \e \f \g}    6
   #{\a \c \f}             7
   #{\a \b \c \d \e \f \g} 8
   #{\a \b \c \d \f \g}    9})

(def all-segments (reduce into (keys segments->number)))

(def number->segments (set/map-invert segments->number))

(def number-of-segments->possible-numbers
  (into {}
        (map (juxt key (comp (partial map val) val)))
        (group-by (comp count key) segments->number)))

(def segment->number-of-occurences
  (frequencies (apply concat (keys segments->number))))

(def number-of-occurences->possible-segments
  (reduce (fn [acc [k v]] (update acc v (fnil conj #{}) k))
          {}
          segment->number-of-occurences))

(defn which-number [wire->possible-segments wires]
  (let [possible-numbers (number-of-segments->possible-numbers (count wires))]
    (if (= (count possible-numbers) 1)
      (first possible-numbers)
      (let [possible-numbers
            (filter (fn [possible-number]
                      (let [number-segments (number->segments possible-number)]
                        (every? (comp seq
                                      (partial set/intersection number-segments)
                                      wire->possible-segments)
                                wires)))
                    possible-numbers)]
        (when (= (count possible-numbers) 1)
          (first possible-numbers))))))

(defn decode-display [{:keys [examples output]}]
  (let [wire->number-of-occurences (frequencies (apply concat examples))]
    (loop [wire->possible-segments
           (into {} (map (juxt identity
                               (comp number-of-occurences->possible-segments
                                     wire->number-of-occurences))
                         all-segments))

           examples
           (into PersistentQueue/EMPTY
                 (sort-by (comp count
                                number-of-segments->possible-numbers
                                count)
                          examples))]
      (let [example  (peek examples)
            examples (pop examples)]
        (if (nil? example)
          (let [wire->segments (into {}
                                     (map (juxt key (comp first val)))
                                     wire->possible-segments)]
            (transduce (map (comp segments->number
                                  set
                                  (partial map wire->segments)))
                       (completing (fn [acc x] (+ (* acc 10) x)))
                       0
                       output))
          (if-let [number (which-number wire->possible-segments example)]
            (let [number-segments (number->segments number)]
              (recur (reduce (fn [wire->possible-segments wire]
                               (update wire->possible-segments
                                       wire
                                       set/intersection
                                       number-segments))
                             wire->possible-segments example)
                     examples))
            (recur wire->possible-segments (conj examples example))))))))

(defn answer-part-2 [parsed-input]
  (transduce (map decode-display) + parsed-input))

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer 1068933))
