(ns advent-of-code.year-2024.day-06
  (:require [advent-of-code.core :as core]
            [advent-of-code.utils :as utils]
            [flatland.ordered.set :refer [ordered-set]]))

(set! *warn-on-reflection* true)

(core/set-date! 2024 6)


;;;; Parse

(defn parse-input [input]
  (utils/parse-2d-map input {\# :obstruction, \. :floor, \^ :guard}))

(core/set-parse-fn! parse-input)


;;;; Part 1

(def direction-vectors {:north [0 -1], :east [1 0], :south [0 1], :west [-1 0]})

(def next-direction
  (let [directions (cycle [:north :east :south :west])]
    (zipmap (take 4 directions) (rest directions))))

(defn guard-step [grid [position direction]]
  (when position
    (let [next-position (mapv + position (direction-vectors direction))
          value         (grid next-position)]
      (case value
        nil             nil
        (:floor :guard) [next-position direction]
        :obstruction    (recur grid [position (next-direction direction)])))))

(defn guard-path [grid guard-state]
  (take-while some? (iterate (partial guard-step grid) guard-state)))

(defn answer-part-1 [{:keys [pois indices]}]
  (count (into #{}
               (map first)
               (guard-path pois [(first (:guard indices)) :north]))))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 41]
  [:puzzle 4964])


;;;; Part 2

(defn add-obstruction [grid position]
  (when (= (grid position) :floor)
    (assoc grid position :obstruction)))

(defn memorize-path [memory future seen]
  (reduce (fn [[memory future] [position direction :as guard-state]]
            (let [future (update future :positions assoc position direction)]
              [(assoc memory guard-state future) future]))
          [memory future]
          (reverse seen)))

(defn memorized-path [memory grid guard-state]
  (if-some [future (memory guard-state)]
    [memory future]
    (apply memorize-path
           memory
           (loop [memory      memory
                  grid        grid
                  guard-state guard-state
                  seen        (ordered-set)]
             (let [next-seen (conj seen guard-state)]
               (if-some [future (memory guard-state)]
                 [future next-seen]
                 (if (seen guard-state)
                   [{:has-loop  true
                     :positions (into {} (drop-while #{guard-state}) seen)}
                    next-seen]
                   (if-some [guard-state (guard-step grid guard-state)]
                     (recur memory grid guard-state next-seen)
                     [{:has-loop false, :positions {}} next-seen]))))))))

(defn back-up [[position direction]]
  [(mapv - position (direction-vectors direction)) direction])

(defn loop-search [memory grid obstruction-position guard-state]
  (let [obstructed-grid (add-obstruction grid obstruction-position)]
    (loop [memory memory
           seen #{}
           guard-state (guard-step obstructed-grid guard-state)]
      (if (seen guard-state)
        [memory true]
        (let [[memory {:keys [has-loop positions]}]
              (memorized-path memory grid guard-state)]
          (if-some [direction (positions obstruction-position)]
            (recur memory
                   (conj seen guard-state)
                   (guard-step obstructed-grid
                               (back-up [obstruction-position direction])))
            [memory has-loop]))))))

(defn answer-part-2 [{:keys [pois indices]}]
  (let [start-position (first (:guard indices))
        original-path  (guard-path pois [start-position :north])]
    (->> original-path
         rest
         (reduce (fn [[memory seen result] [position :as guard-state]]
                   (if (seen position)
                     [memory seen result]
                     (let [[memory loop-found]
                           (loop-search memory
                                        pois
                                        position
                                        (back-up guard-state))]
                       [memory
                        (conj seen position)
                        (cond-> result loop-found inc)])))
                 [{} #{start-position} 0])
         last)))

(core/set-answer-fn! 2 answer-part-2
    [:sample1 6]
    [:puzzle [not= 1857] [not= 1757] [not= 1690] 1740])
