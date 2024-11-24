(ns advent-of-code.year-2021.day-22
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2021 22)


;;;; Parse

(def pattern #"(on|off) x=(.+)\.\.(.+),y=([\-0-9]+)\.\.(.+),z=(.+)\.\.(.+)")

(defn parse-input [input]
  (map (fn [s]
         (let [[_ state & coords] (re-matches pattern s)]
           [(keyword state) (->> coords
                                 (map parse-long)
                                 (partition 2)
                                 (mapv vec))]))
       (string/split-lines input)))

(core/set-parse-fn! parse-input)


;;;; Part 1

(def volumes-intersect?
  (comp (partial every? (fn [[[a-start a-end] [b-start b-end]]]
                          (or (<= a-start b-start a-end)
                              (<= a-start b-end a-end)
                              (<= b-start a-start b-end)
                              (<= b-start a-end b-end))))
        (partial map vector)))

(defn volume-difference
  [[[ax-start ax-end] [ay-start ay-end] [az-start az-end]]
   [[bx-start bx-end] [by-start by-end] [bz-start bz-end] :as b-volume]]
  (let [z-ranges (filter (partial apply <=) [[az-start (dec bz-start)]
                                             [(max az-start bz-start)
                                              (min bz-end az-end)]
                                             [(inc bz-end) az-end]])
        y-ranges (filter (partial apply <=) [[ay-start (dec by-start)]
                                             [(max ay-start by-start)
                                              (min by-end ay-end)]
                                             [(inc by-end) ay-end]])
        x-ranges (filter (partial apply <=) [[ax-start (dec bx-start)]
                                             [(max ax-start bx-start)
                                              (min bx-end ax-end)]
                                             [(inc bx-end) ax-end]])]
    (for [z-range z-ranges
          y-range y-ranges
          x-range x-ranges
          :let    [volume [x-range y-range z-range]]
          :when   (not (volumes-intersect? volume b-volume))]
      volume)))

(defn on-cubes-count [instructions]
  (->> instructions
       (reduce (fn [volumes [state volume]]
                 (let [intersecting-volumes (filter (partial volumes-intersect?
                                                             volume)
                                                    volumes)]
                   (cond-> (into (apply disj volumes intersecting-volumes)
                                 (mapcat #(volume-difference % volume))
                                 intersecting-volumes)
                     (= state :on) (conj volume))))
               #{})
       (transduce (map (fn [volume]
                         (apply * (map (fn [[start end]]
                                         (- (inc end) start)) volume))))
                  +)))

(defn answer-part-1 [parsed-input]
  (->> parsed-input
       (filter (comp (partial every? #(<= -50 % 50)) flatten rest))
       on-cubes-count))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 610196])


;;;; Part 2

(defn answer-part-2 [parsed-input]
  (on-cubes-count parsed-input))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 1282401587270826])
