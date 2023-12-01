(ns advent-of-code.year-2022.day-24
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]
            [medley.core :as medley]))

(def input (core/get-input))

(def character->blizzard-direction {\> :right, \v :down, \< :left, \^ :up})

(defn parse-input [input]
  (let [valley-map (mapv (fn [s] (subs s 1 (dec (count s))))
                         (butlast (rest (string/split-lines input))))
        height     (count valley-map)
        width      (count (first valley-map))]
    {:height    height
     :width     width
     :blizzards (for [row    (range height)
                      column (range width)
                      :let   [position [row column]
                              character (get-in valley-map position)]
                      :when  (not= character \.)]
                  [position (character->blizzard-direction character)])}))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

(def direction->delta {:right [0 1], :down [1 0], :left [0 -1], :up [-1 0]})
(def possible-deltas (cons [0 0] (vals direction->delta)))

(defn blizzard-cycle [height width position direction]
  (->> position
       (iterate (partial mapv + (direction->delta direction)))
       (map #(mapv mod % [height width]))
       (take (case direction (:left :right) width, (:up :down) height))
       cycle))

(defn valley-maps [blizzard-cycles]
  (map (partial into #{} (map first))
       (iterate (partial map rest) blizzard-cycles)))

(defn next-possible-positions [position valley-map height width]
  (sequence (comp (map (partial mapv + position))
                  (remove valley-map)
                  (filter (some-fn #{[-1 0] [height (dec width)]}
                                   (fn [[row column]]
                                     (and (< -1 row height)
                                          (< -1 column width))))))
            possible-deltas))

(defn all-possible-positions [possible-positions valley-map height width]
  (into #{}
        (mapcat #(next-possible-positions % valley-map height width))
        possible-positions))

(defn minutes-to [height width valley-maps from to]
  (let [positions-at    (reductions #(all-possible-positions %1 %2 height width)
                                    #{from}
                                    (rest valley-maps))]
    (->> positions-at
         (take-while (complement #(contains? % to)))
         count)))

(defn answer-part-1 [{:keys [height width blizzards]}]
  (let [blizzard-cycles (map (partial apply blizzard-cycle height width)
                             blizzards)
        valley-maps     (valley-maps blizzard-cycles)]
    (minutes-to height width valley-maps [-1 0] [height (dec width)])))

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 249))


;;; Part 2
;;; ============================================================================

(defn answer-part-2 [{:keys [height width blizzards]}]
  (let [start           [-1 0]
        end             [height (dec width)]
        blizzard-cycles (map (partial apply blizzard-cycle height width)
                             blizzards)
        valley-maps     (valley-maps blizzard-cycles)]
    (first (reduce (fn [[minutes valley-maps] [a b]]
                     (let [minutes* (minutes-to height width valley-maps a b)]
                       [(+ minutes minutes*) (drop minutes* valley-maps)]))
                   [0 valley-maps]
                   [[start end] [end start] [start end]]))))

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer 735))
