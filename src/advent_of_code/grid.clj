(ns advent-of-code.grid
  (:require [clojure.math.combinatorics :as combo]
            [clojure.set :as set]
            [clojure.string :as string]
            [medley.core :as medley]))

(def origin [0 0])

(def n [0 -1])
(def e [1 0])
(def s [0 1])
(def w [-1 0])
(def cardinals [n e s w])

(def ne [1 -1])
(def se [1 1])
(def nw [-1 -1])
(def sw [-1 1])
(def box [n e s w ne se nw sw])

(defn parse [input legend]
  (let [lines  (string/split-lines input)
        height (count lines)
        width  (count (first lines))
        pois   (into {}
                     (keep (fn [[x y]]
                             (when-some [type (legend (get-in lines [y x]))]
                               [[x y] type])))
                     (combo/cartesian-product (range width) (range height)))]
    {:height  height
     :width   width
     :pois    pois
     :indices (medley/collate-by val
                                 (fn [positions [coordinates]]
                                   (conj positions coordinates))
                                 (fn [[coordinates]] #{coordinates})
                                 pois)}))

(defn grid-string [{:keys [height width pois]} legend]
  (let [symbol->character (set/map-invert legend)]
    (string/join "\n" (map (fn [y]
                             (string/join (map (fn [x]
                                                 (symbol->character (pois [x y])
                                                                    \space))
                                               (range width))))
                           (range height)))))

(defn neighbors [coordinates] (map (partial mapv + coordinates) cardinals))

(defn manhattan-distance [[ax ay] [bx by]] (+ (abs (- ax bx)) (abs (- ay by))))

(defn shoelace
  "Given a list of vertices in the [x y] format representing a polygon,
   calculate the area of the polygon. The list must be of contiguous points. The
   first and last vertices must be equal for a loop to be formed."
  [points]
  (/ (abs (reduce (fn [res [[a b] [c d]]] (+ res (- (* a d) (* b c))))
                  0
                  (partition 2 1 points)))
     2))

(defn area-vertices
  "Given a list of vertices in the [x y] format representing a polygon,
   calculate the number of vertices in the polygon (edge included). The list
   must be of contiguous points. The first and last vertices must be equal for a
   loop to be formed."
  [points]
  (+ 1
     (shoelace points)
     (/ (reduce #(+ %1 (apply manhattan-distance %2)) 0 (partition 2 1 points))
        2)))
