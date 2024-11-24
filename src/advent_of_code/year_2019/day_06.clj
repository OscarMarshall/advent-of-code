(ns advent-of-code.year-2019.day-06
  (:require [advent-of-code.core :as core]
            [advent-of-code.utils :as utils]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2019 6)


;;;; Parse

(defn parse-input [input]
  (into {}
        (map (fn [line] (vec (reverse (rest (re-matches #"(.*)\)(.*)" line))))))
        (string/split-lines input)))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn all-orbits [object direct-orbits]
            (case object
              "COM" 0
              (+ 1 (all-orbits (direct-orbits object) direct-orbits))))

(defn answer-part-1 [direct-orbits]
  (apply + (map #(all-orbits % direct-orbits) (keys direct-orbits))))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 42]
  [:puzzle 295834])


;;;; Part 2

(defn answer-part-2 [orbiting]
  (let [orbiters (reduce (fn [orbiters [orbiter orbitee]]
                           (update orbiters orbitee (fnil conj #{}) orbiter))
                         {}
                         orbiting)
        target   (orbiting "SAN")]
    (some (fn [[object steps]] (when (= object target) steps))
          (utils/djkstra-walk (fn [object]
                                (conj (orbiters object) (orbiting object)))
                        (orbiting "YOU")))))

(core/set-answer-fn! 2 answer-part-2
  [:sample2 4]
  [:puzzle 361])
