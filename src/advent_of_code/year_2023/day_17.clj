(ns advent-of-code.year-2023.day-17
  (:require [advent-of-code.core :as core]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)

(core/set-date! 2023 17)


;;;; Parse

(defn parse-input [input]
  (mapv (fn [row] (mapv #(parse-long (str %)) row)) (str/split-lines input)))

(core/set-parse-fn! parse-input)

;;;; Part 1

(def previous-direction->directions
  {nil    [:right :down]
   :up    [:left :right]
   :right [:up :down]
   :down  [:left :right]
   :left  [:up :down]})

(def direction->vector
  {:up    [-1 0]
   :right [0 1]
   :down  [1 0]
   :left  [0 -1]})

(defn next-coordinates [coordinates direction]
  (mapv + coordinates (direction->vector direction)))

;; A state is defined as a vector of the priority (lower will be processed
;; first), a unique key, and any extra info you'd like to include.
(defn dijkstra-seq
  ([state->next-states starting-state]
   (dijkstra-seq state->next-states (sorted-set starting-state) #{}))
  ([state->next-states states seen]
   (lazy-seq (when-some [state (first states)]
               (let [states (disj states state)
                     key    (nth state 1)]
                 (if (seen key)
                   (dijkstra-seq state->next-states states seen)
                   (let [seen (conj seen key)]
                     (cons state
                           (dijkstra-seq state->next-states
                                         (into states
                                               (state->next-states state))
                                         seen)))))))))

(defn make-state->next-states [grid min max]
  (fn [[heat-loss-acc [coordinates direction]]]
    (mapcat (fn [direction]
              (let [coordinateses (->> (iterate #(next-coordinates % direction)
                                                coordinates)
                                       rest
                                       (take max))
                    heat-losses   (rest (reductions + 0 (keep #(get-in grid %)
                                                              coordinateses)))]
                (sequence (comp (map (fn [heat-loss coordinates]
                                       [(+ heat-loss-acc heat-loss)
                                        [coordinates direction]]))
                                (drop (dec min)))
                          heat-losses
                          coordinateses)))
            (previous-direction->directions direction))))

(defn lowest-heat-loss [grid min max]
  (let [target [(dec (count grid)) (dec (count (first grid)))]]
    (some (fn [[heat-loss [coordinates]]]
            (when (= coordinates target) heat-loss))
          (dijkstra-seq (make-state->next-states grid min max)
                        [0 [[0 0] nil]]))))

(defn answer-part-1 [grid] (lowest-heat-loss grid 1 3))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 102]
  [:puzzle 668])


;;;; Part 2

(defn answer-part-2 [grid] (lowest-heat-loss grid 4 10))

(core/set-answer-fn! 2 answer-part-2
  [:sample1 94]
  [:sample2 71]
  [:puzzle 788])
