(ns advent-of-code.year-2019.day-10
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2019 10)


;;;; Parse

(defn parse-input [input]
  (into #{}
        (comp (map-indexed (fn [y line]
                             (map-indexed (fn [x c]
                                            (when (= c \#) [x y])) line)))
              cat
              (filter some?))
        (string/split-lines input)))


(core/set-parse-fn! parse-input)


;;;; Part 1

(defn gcd [a b] (if (zero? b) a (recur b (mod a b))))

(defn visible-asteroid-vectors [[ax ay, :as asteroid] asteroids]
  (group-by (fn [[bx by]]
              (let [dx (- bx ax)
                    dy (- by ay)
                    d  (gcd (abs dx) (abs dy))]
                [(/ dx d) (/ dy d)]))
            (disj asteroids asteroid)))

(defn answer-part-1 [asteroids]
  (apply max
         (map (comp count #(visible-asteroid-vectors % asteroids)) asteroids)))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 8]
  [:sample2 33]
  [:sample3 35]
  [:sample4 41]
  [:sample5 210]
  [:puzzle 214])


;;;; Part 2

(defn answer-part-2 [asteroids]
  (let [asteroid-vectors (apply max-key
                                count
                                (map #(visible-asteroid-vectors % asteroids)
                                     asteroids))]
    (-> asteroid-vectors
        (->> (sort-by (fn [[[x y]]]
                        (mod (- (- (Math/atan2 (- y) x) (* 1/2 Math/PI)))
                             (* 2 Math/PI)))))
        (nth (dec 200))
        val
        first
        (->> (reduce (fn [x y] (+ (* x 100) y)))))))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 502])
