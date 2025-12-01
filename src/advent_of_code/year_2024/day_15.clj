(ns advent-of-code.year-2024.day-15
  (:require [advent-of-code.core :as core]
            [advent-of-code.utils :as utils]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2024 15)


;;;; Parse

(defn parse-warehouse [warehouse-map]
  (:pois (utils/parse-2d-map warehouse-map {\# :wall, \O :box, \@ :robot})))

(defn parse-movements [movements]
  (keep {\^ :up, \v :down, \< :left, \> :right} movements))

(defn parse-input [input]
  (let [[warehouse-map movements] (string/split input #"\n\n" 2)
        warehouse                 (parse-warehouse warehouse-map)]
    {:warehouse warehouse
     :movements (parse-movements movements)}))

(core/set-parse-fn! parse-input)


;;;; Part 1

(def movement->vector
  {:up    [0 -1]
   :down  [0 1]
   :left  [-1 0]
   :right [1 0]})

(defn push [[warehouse position] movement]
  (let [type (warehouse position)]
    (case type
      :wall nil
      nil   [warehouse nil]
      (let [next-position (mapv + position (movement->vector movement))]
        (when-some [[warehouse] (push [warehouse next-position] movement)]
          (when-some [[warehouse] (if (#{:up :down} movement)
                                    (case type
                                      :box-left  (push [warehouse
                                                        (mapv +
                                                              next-position
                                                              [1 0])]
                                                       movement)
                                      :box-right (push [warehouse
                                                        (mapv +
                                                              next-position
                                                              [-1 0])]
                                                       movement)
                                      [warehouse])
                                    [warehouse])]
            [(cond-> (-> warehouse
                         (dissoc position)
                         (assoc next-position type))
               (#{:up :down} movement)
               (cond->
                   (= type :box-left)  (-> (dissoc (mapv + position [1 0]))
                                           (assoc (mapv + next-position [1 0])
                                                  :box-right))
                   (= type :box-right) (-> (dissoc (mapv + position [-1 0]))
                                           (assoc (mapv + next-position [-1 0])
                                                  :box-left))))
             next-position]))))))

(defn answer-part-1 [{:keys [warehouse movements]}]
  (let [robot-position (some (fn [[position type]]
                               (when (= type :robot) position))
                             warehouse)]
    (transduce (keep (fn [[[x y] type]]
                       (when (#{:box :box-left} type) (+ x (* 100 y)))))
               +
               (first (reduce (fn [state movement]
                                (or (push state movement) state))
                              [warehouse robot-position] movements)))))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 10092]
  [:sample2 2028]
  [:puzzle 1514333])


;;;; Part 2

(defn widen-warehouse [warehouse]
  (into {}
        (mapcat (fn [[[x y] type]]
                  (let [xx (* x 2)]
                    (case type
                      :wall  [[[xx y] :wall] [[(inc xx) y] :wall]]
                      :box   [[[xx y] :box-left] [[(inc xx) y] :box-right]]
                      :robot [[[xx y] :robot]]))))
        warehouse))

(defn answer-part-2 [input]
  (answer-part-1 (update input :warehouse widen-warehouse)))

(core/set-answer-fn! 2 answer-part-2
  [:sample1 9021]
  [:puzzle #_(get-in @core/state [:part2 :outputs :puzzle :result])])
