(ns advent-of-code.year-2021.day-02
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2021 2)


;;;; Parse

(defn parse-input [input]
  (map (fn [s]
         (let [[direction amount] (string/split s #" ")]
           [(keyword direction) (parse-long amount)]))
       (string/split-lines input)))

(core/set-parse-fn! parse-input)


;;;; Part 1

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

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 2027977])


;;;; Part 2

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

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 1903644897])
