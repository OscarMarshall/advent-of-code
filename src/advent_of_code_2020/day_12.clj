(ns advent-of-code-2020.day-12
  (:require [advent-of-code-2020.core :as core]
            [clojure.string :as string]))

;;; Part 1
;;; ============================================================================

(def input (core/get-input))

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
            (Long/parseLong n)]))
       (string/split-lines input)))

(def parsed-input (parse-input input))

(defmulti do-command (fn [_ [command _]] command))
(defmethod do-command :north [state [_ n]]
  (update state :north + n))
(defmethod do-command :south [state [_ n]]
  (update state :north - n))
(defmethod do-command :east [state [_ n]]
  (update state :east + n))
(defmethod do-command :west [state [_ n]]
  (update state :east - n))
(def facing->direction [:east :north :west :south])
(defmethod do-command :left [state [_ n]]
  (update state :facing #(mod (+ % (/ n 90)) 4)))
(defmethod do-command :right [state [_ n]]
  (update state :facing #(mod (- % (/ n 90)) 4)))
(defmethod do-command :forward [{:as state, :keys [facing]} [_ n]]
  (do-command state [(facing->direction facing) n]))

(defn answer-part-1 [parsed-input]
  (let [{:keys [north east]} (reduce do-command
                                     {:east 0, :facing 0, :north 0}
                                     parsed-input)]
    (+ (cond-> north (neg? north) -) (cond-> east (neg? east) -))))

(def part-1-answer (answer-part-1 parsed-input))

(comment
  part-1-answer
  ;; => 420
  )

;;; Part 2
;;; ============================================================================

(defmulti do-command2 (fn [_ [command _]] command))
(defmethod do-command2 :north [state [_ n]]
  (update state :waypoint-north + n))
(defmethod do-command2 :south [state [_ n]]
  (update state :waypoint-north - n))
(defmethod do-command2 :east [state [_ n]]
  (update state :waypoint-east + n))
(defmethod do-command2 :west [state [_ n]]
  (update state :waypoint-east - n))
(defmethod do-command2 :left [{:as state, :keys [waypoint-east waypoint-north]}
                              [_ n]]
  (if (zero? n)
    state
    (recur (assoc state
                  :waypoint-east (- waypoint-north)
                  :waypoint-north waypoint-east)
           [:left (- n 90)])))
(defmethod do-command2 :right [{:as state, :keys [waypoint-east waypoint-north]}
                              [_ n]]
  (if (zero? n)
    state
    (recur (assoc state
                  :waypoint-east waypoint-north
                  :waypoint-north (- waypoint-east))
           [:right (- n 90)])))
(defmethod do-command2 :forward [{:as state, :keys [waypoint-east
                                                    waypoint-north]}
                                 [_ n]]
  (-> state
      (update :east + (* waypoint-east n))
      (update :north + (* waypoint-north n))))

(defn answer-part-2 [parsed-input]
  (let [{:keys [north east]} (reduce do-command2
                                     {:east 0
                                      :north 0
                                      :waypoint-east 10
                                      :waypoint-north 1}
                                     parsed-input)]
    (+ (cond-> north (neg? north) -) (cond-> east (neg? east) -))))

(def part-2-answer (answer-part-2 parsed-input))

(comment
  part-2-answer
  ;; => 42073
  )
