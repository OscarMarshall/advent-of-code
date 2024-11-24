(ns advent-of-code.year-2020.day-13
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2020 13)


;;;; Parse

(defn parse-input [input]
  (let [[earliest buses] (string/split-lines input)]
    [(parse-long earliest)
     (map (some-fn #{"x"} parse-long) (string/split buses #","))]))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn answer-part-1 [parsed-input]
  (let [[earliest buses] parsed-input
        [id pick-up]     (transduce (comp (remove #{"x"})
                                          (map (fn [id]
                                                 [id
                                                  (some #(when (>= % earliest)
                                                           %)
                                                        (iterate #(+ % id)
                                                                 0))])))
                                    (completing (fn [[_ pick-up-a :as a]
                                                     [_ pick-up-b :as b]]
                                                  (if (< pick-up-a pick-up-b)
                                                    a
                                                    b)))
                                    [nil Long/MAX_VALUE]
                                    buses)]
    (* id (- pick-up earliest))))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 4938])


;;;; Part 2

(defn answer-part-2 [parsed-input]
  (let [[_ buses] parsed-input]
    (nth (transduce
          (comp (map-indexed vector)
                (remove (comp #{"x"} #(nth % 1))))
          (completing (fn [[period earliest] [i id]]
                        (loop [earliest (+ earliest i)]
                          (if (zero? (mod earliest id))
                            [(* period id) (- earliest i)]
                            (recur (+ earliest period))))))
          [1 0]
          buses)
         1)))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 230903629977901])
