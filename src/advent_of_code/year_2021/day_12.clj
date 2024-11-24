(ns advent-of-code.year-2021.day-12
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2021 12)


;;;; Parse

(defn parse-input [input]
  (map (comp vec rest (partial re-matches #"(.*)-(.*)"))
       (string/split-lines input)))

(core/set-parse-fn! parse-input)


;;;; Part 1

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

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 4912])


;;;; Part 2

(defn answer-part-2 [parsed-input]
  (count (paths-to-end parsed-input 1)))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 150004])
