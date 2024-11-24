(ns advent-of-code.year-2023.day-22
  (:require [advent-of-code.core :as core]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)

(core/set-date! 2023 22)


;;;; Parse

(defn parse-input [input]
  (map (fn [brick]
         (mapv #(mapv parse-long (str/split % #",")) (str/split brick #"~")))
       (str/split-lines input)))

(core/set-parse-fn! parse-input)

;;;; Part 1

(defn brick->voxels [[[x1 y1 z1] [x2 y2 z2]]]
  (cond
    (not= x1 x2) (mapv (fn [x] [x y1 z1]) (range x1 (inc x2)))
    (not= y1 y2) (mapv (fn [y] [x1 y z1]) (range y1 (inc y2)))
    :else        (mapv (fn [z] [x1 y1 z]) (range z1 (inc z2)))))

(defn voxel-column [[x y _]] [x y])

(defn voxel-height [[_ _ z]] z)

(defn drop-brick [[resting-bricks highest-points dropped-bricks] voxels]
  (let [lowest-voxel  (apply min (map voxel-height voxels))
        highest-point (apply max
                             0
                             (keep #(highest-points (voxel-column %)) voxels))
        drop-height   (dec (- lowest-voxel highest-point))
        dropped-brick (map #(update % 2 - drop-height) voxels)]
    [(conj resting-bricks dropped-brick)
     (into highest-points
           (map (juxt voxel-column voxel-height))
           dropped-brick)
     (cond-> dropped-bricks (pos? drop-height) inc)]))

(defn drop-bricks [bricks]
  (reduce drop-brick [[] {} 0] bricks))

(defn answer-part-1 [bricks]
  (let [dropped-bricks (->> bricks
                            (sort-by #(get-in % [0 2]))
                            (mapv brick->voxels)
                            drop-bricks
                            first)]
    (count (filter #(let [bricks1 (concat (subvec dropped-bricks 0 %)
                                          (subvec dropped-bricks (inc %)))]
                      (= (first (drop-bricks bricks1)) bricks1))
                   (range (count dropped-bricks))))))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 5]
  [:puzzle 461])


;;;; Part 2

(defn answer-part-2 [bricks]
  (let [dropped-bricks (->> bricks
                            (sort-by #(get-in % [0 2]))
                            (mapv brick->voxels)
                            drop-bricks
                            first)]
    (transduce (map #(nth (drop-bricks (concat (subvec dropped-bricks 0 %)
                                               (subvec dropped-bricks (inc %))))
                          2))
               +
               (range (count dropped-bricks)))))

(core/set-answer-fn! 2 answer-part-2
  [:sample1 7]
  [:puzzle 74074])
