(ns advent-of-code.year-2021.day-20
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(def input (core/get-input *file*))

(defn parse-input [input]
  (let [[[algorithm] _ image] (partition-by #{""} (string/split-lines input))]
    {:algorithm algorithm
     :image     (vec image)}))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

(def flip-pixel {\. \#, \# \.})

(defn process-image [image]
  (let [height (count image)
        width  (count (first image))]
    {:background \.
     :points     (into #{} (for [y     (range height)
                                 x     (range width)
                                 :when (= (get-in image [y x]) \#)]
                             [x y]))}))

(defn image-range [{:keys [points]}]
  (let [[x1 x2 y1 y2] (reduce (fn [[x1 x2 y1 y2] [x y]]
                                [(min x1 x) (max x2 x) (min y1 y) (max y2 y)])
                              [0 0 0 0]
                              points)]
    [[x1 (inc x2)] [y1 (inc y2)]]))

(defn pixels->pixel [pixels algorithm]
  (nth algorithm (reduce (fn [acc pixel]
                           (cond-> (bit-shift-left acc 1) (= pixel \#) inc))
                         0
                         pixels)))

(defn step-point [{:keys [background points]} algorithm [x y]]
  (let [foreground (flip-pixel background)]
    (pixels->pixel (for [y (range (dec y) (+ y 2))
                         x (range (dec x) (+ x 2))]
                     (if (points [x y]) foreground background))
                   algorithm)))

(defn step-image [{:keys [background], :as image} algorithm]
  (let [[x-range y-range] (map (juxt (comp dec first) (comp inc second))
                               (image-range image))
        background        (pixels->pixel (repeat 9 background) algorithm)]
    {:background background
     :points     (into #{}
                       (for [y     (apply range y-range)
                             x     (apply range x-range)
                             :let  [point (step-point image algorithm [x y])]
                             :when (not= point background)]
                         [x y]))}))

(defn answer-part-1 [{:keys [algorithm image]}]
  (count (:points (nth (iterate #(step-image % algorithm)
                                (process-image image))

                       2))))

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer part-1-answer))


;;; Part 2
;;; ============================================================================

(defn answer-part-2 [{:keys [algorithm image]}]
  (count (:points (nth (iterate #(step-image % algorithm)
                                (process-image image))

                       50))))

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer 20097))
