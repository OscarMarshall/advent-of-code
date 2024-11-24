(ns advent-of-code.year-2021.day-08
  (:require [advent-of-code.core :as core]
            [clojure.set :as set]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2021 8)


;;;; Parse

(defn parse-signal-patterns [signal-patterns]
  (map (partial into #{}) (string/split signal-patterns #" ")))

(defn parse-input [input]
  (map (fn [s]
         (let [[examples output] (map parse-signal-patterns
                                      (string/split s #" \| "))]
           {:examples examples, :output output}))
       (string/split-lines input)))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn answer-part-1 [parsed-input]
  (count (into ()
               (comp (mapcat :output) (map count) (filter #{2 3 4 7}))
               parsed-input)))

(core/set-answer-fn! 1 answer-part-1
                     [:puzzle 342])


;;;; Part 2

(def number->segments
  {0 #{\a \b \c \e \f \g}
   1 #{\c \f}
   2 #{\a \c \d \e \g}
   3 #{\a \c \d \f \g}
   4 #{\b \c \d \f}
   5 #{\a \b \d \f \g}
   6 #{\a \b \d \e \f \g}
   7 #{\a \c \f}
   8 #{\a \b \c \d \e \f \g}
   9 #{\a \b \c \d \f \g}})

(def four-segments (number->segments 4))

(def segments->number (set/map-invert number->segments))

(def frequency->segments
  (reduce (fn [acc [k v]] (update acc v (fnil conj #{}) k))
          {}
          (frequencies (into () (mapcat val) number->segments))))

(defn decode-display [{:keys [examples output]}]
  (let [four-wires     (some (fn [example] (when (= (count example) 4) example))
                             examples)
        wire->segments (into {}
                             (map (fn [[wire frequency]]
                                    (let [segments (frequency->segments
                                                    frequency)]
                                      [wire (first (cond-> segments
                                                     (> (count segments) 1)
                                                     ((if (four-wires wire)
                                                        set/intersection
                                                        set/difference)
                                                      four-segments)))])))
                             (frequencies (apply concat examples)))]
    (transduce (map (comp segments->number
                          set
                          (partial map wire->segments)))
               (completing (fn [acc x] (+ (* acc 10) x)))
               0
               output)))

(defn answer-part-2 [parsed-input]
  (transduce (map decode-display) + parsed-input))

(core/set-answer-fn! 2 answer-part-2
                     [:puzzle 1068933])
