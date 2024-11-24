(ns advent-of-code.year-2019.day-03
  (:require [advent-of-code.core :as core]
            [clojure.set :as set]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2019 3)


;;;; Parse

(defn parse-instruction [instruction]
  [(case (first instruction) \U :up, \R :right, \D :down, \L :left)
   (parse-long (subs instruction 1))])

(defn parse-input [input]
  (map (comp (partial map parse-instruction) #(string/split % #","))
       (string/split-lines input)))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn span-positions [[x y] distance [direction amount]]
  (into []
        (comp (map (juxt (case direction
                           :up    (fn [offset] [(+ x offset) y])
                           :right (fn [offset] [x (+ y offset)])
                           :down  (fn [offset] [(- x offset) y])
                           :left  (fn [offset] [x (- y offset)]))
                         (partial + distance)))
              (map (fn [[position distance]]
                     (with-meta position {:distance distance}))))
        (range 1 (inc amount))))

(defn trace-wire [wire]
  (->> wire
       (reduce (fn [[acc position distance] instruction]
                 (let [positions (span-positions position distance instruction)]
                   [(into acc positions)
                    (last positions)
                    (:distance (meta (last positions)))]))
               [#{} [0 0] 0])
       first))

(defn answer-part-1 [wires]
  (apply min (map (fn [[x y]] (+ (abs x) (abs y)))
                  (apply set/intersection (map trace-wire wires)))))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 159]
  [:sample2 135]
  [:puzzle 260])


;;;; Part 2

(defn answer-part-2 [wires]
  (let [traces (map trace-wire wires)]
    (apply min (map (fn [position]
                      (->> traces
                           (map (fn [trace]
                                  (:distance (meta (trace position)))))
                           (apply +)))
                    (apply set/intersection traces)))))

(core/set-answer-fn! 2 answer-part-2
  [:sample1 610]
  [:sample2 410]
  [:puzzle 15612])
