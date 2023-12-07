(ns advent-of-code.year-2021.day-02
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(def input (core/get-input *file*))

(defn parse-input [input]
  (map (fn [s]
         (let [[direction amount] (string/split s #" ")]
           [(keyword direction) (Long/parseLong amount)]))
       (string/split-lines input)))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

(defn move [position [direction amount]]
  (case direction
    :forward (update position :horizontal + amount)
    :down    (update position :depth + amount)
    :up      (update position :depth - amount)))

(defn answer-part-1 [parsed-input]
  (let [{:keys [horizontal depth]} (reduce move
                                           {:horizontal 0, :depth 0}
                                           parsed-input)]
    (* horizontal depth)))

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 2027977))


;;; Part 2
;;; ============================================================================

(defn move2 [{:as position, :keys [aim]} [direction amount]]
  (case direction
    :down    (update position :aim + amount)
    :up      (update position :aim - amount)
    :forward (-> position
                 (update :horizontal + amount)
                 (update :depth + (* amount aim)))))

(defn answer-part-2 [parsed-input]
  (let [{:keys [horizontal depth]} (reduce move2
                                           {:horizontal 0, :depth 0, :aim 0}
                                           parsed-input)]
    (* horizontal depth)))

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer 1903644897))
