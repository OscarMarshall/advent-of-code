(ns advent-of-code.year-2021.day-12
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(def input (core/get-input))

(defn parse-input [input]
  (map (comp vec rest (partial re-matches #"(.*)-(.*)"))
       (string/split-lines input)))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

(defn paths-to-end [parsed-input small-cave-revisits]
  (let [connections (into {}
                          (map (juxt key (comp (partial map second) val)))
                          (group-by first
                                    (mapcat (juxt identity (comp vec reverse))
                                            parsed-input)))
        caves       (into #{} cat parsed-input)
        small-caves (into #{} (filter (partial re-matches #"[a-z]+")) caves)]
    (letfn [(paths-to-end* [cave seen-small-caves small-cave-revisits]
              (if (= cave "end")
                ['("end")]
                (mapcat (let [seen-small-caves
                              (cond-> seen-small-caves
                                (small-caves cave) (conj cave))]
                          (fn [next-cave]
                            (map #(conj % cave)
                                 (if (seen-small-caves next-cave)
                                   (if (or (zero? small-cave-revisits)
                                           (= next-cave "start"))
                                     ()
                                     (paths-to-end* next-cave
                                                    seen-small-caves
                                                    (dec small-cave-revisits)))
                                   (paths-to-end* next-cave
                                                  seen-small-caves
                                                  small-cave-revisits)))))
                        (connections cave))))]
      (into #{} (paths-to-end* "start" #{} small-cave-revisits)))))

(defn answer-part-1 [parsed-input]
  (count (paths-to-end parsed-input 0)))

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 4912))


;;; Part 2
;;; ============================================================================

(defn answer-part-2 [parsed-input]
  (count (paths-to-end parsed-input 1)))

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer 150004))
