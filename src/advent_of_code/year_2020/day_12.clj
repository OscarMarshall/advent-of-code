(ns advent-of-code.year-2020.day-12
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2020 12)


;;;; Parse

(defn parse-input [input]
  (map (fn [line]
         (let [[_ command n] (re-matches #"(.)(\d+)" line)]
           [(case command
              "N" :north
              "S" :south
              "E" :east
              "W" :west
              "L" :left
              "R" :right
              "F" :forward)
            (parse-long n)]))
       (string/split-lines input)))

(core/set-parse-fn! parse-input)


;;;; Part 1

(def facing->direction [:east :north :west :south])

(defn do-command [{:keys [facing], :as state} [command n]]
  (case command
    :north   (update state :north + n)
    :south   (update state :north - n)
    :east    (update state :east + n)
    :west    (update state :east - n)
    :left    (update state :facing #(mod (+ % (/ n 90)) 4))
    :right   (update state :facing #(mod (- % (/ n 90)) 4))
    :forward (do-command state [(facing->direction facing) n])))

(defn answer-part-1 [parsed-input]
  (let [{:keys [north east]} (reduce do-command
                                     {:east 0, :facing 0, :north 0}
                                     parsed-input)]
    (+ (cond-> north (neg? north) -) (cond-> east (neg? east) -))))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 420])


;;;; Part 2

(defn do-command2 [{:keys [waypoint-east waypoint-north], :as state}
                   [command n]]
  (case command
    :north   (update state :waypoint-north + n)
    :south   (update state :waypoint-north - n)
    :east    (update state :waypoint-east + n)
    :west    (update state :waypoint-east - n)
    :left    (if (zero? n)
               state
               (recur (assoc state
                             :waypoint-east (- waypoint-north)
                             :waypoint-north waypoint-east)
                      [:left (- n 90)]))
    :right   (if (zero? n)
               state
               (recur (assoc state
                             :waypoint-east waypoint-north
                             :waypoint-north (- waypoint-east))
                      [:right (- n 90)]))
    :forward (-> state
                 (update :east + (* waypoint-east n))
                 (update :north + (* waypoint-north n)))))

(defn answer-part-2 [parsed-input]
  (let [{:keys [north east]} (reduce do-command2
                                     {:east 0
                                      :north 0
                                      :waypoint-east 10
                                      :waypoint-north 1}
                                     parsed-input)]
    (+ (cond-> north (neg? north) -) (cond-> east (neg? east) -))))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 42073])
