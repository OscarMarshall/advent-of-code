(ns advent-of-code-2020.day-20
  (:require [advent-of-code-2020.core :as core]
            [clojure.set :as set]
            [clojure.string :as string]))

;;; Part 1
;;; ============================================================================

(def input (core/get-input))

(defn parse-input [input]
  (into {}
        (map (fn [tile]
               (let [lines (string/split-lines tile)
                     id    (Long/parseLong (second (re-matches #"Tile (\d+):"
                                                               (first lines))))]
                 [id {:id id, :image (mapv vec (rest lines))}])))
        (string/split input #"\n\n")))

(def parsed-input (parse-input input))

(def top (comp first :image))
(def bottom (comp last :image))
(def left (comp vec (partial map first) :image))
(def right (comp vec (partial map last) :image))

(defn tile->sides [tile]
  (map #(% tile) [top right bottom left]))

(defn process-sides [tiles]
  (into {} (map (juxt key (comp tile->sides val))) tiles))

(defn make-directory [id->sides]
  (apply merge-with
         set/union
         (map (fn [[id sides]]
                (zipmap (concat sides (map (comp vec reverse) sides))
                        (repeat #{id})))
              id->sides)))

(defn border? [side directory]
  (= (count (directory side)) 1))

(defn find-corners [id->sides directory]
  (filter (fn [id]
            (= (count (filter #(border? % directory) (id->sides id))) 2))
          (into #{}
                (comp (filter (comp #{1} count))
                      (map first))
                (vals directory))))

(defn answer-part-1 [parsed-input]
  (let [id->sides (process-sides parsed-input)
        directory (make-directory id->sides)]
    (apply * (find-corners id->sides directory))))

(def part-1-answer (answer-part-1 parsed-input))

(comment
  part-1-answer
  ;; => 79412832860579
  )

;;; Part 2
;;; ============================================================================

(defn rotate-image [tile]
  (update tile :image (fn [image] (apply mapv vector (reverse image)))))

(defn flip-image [tile]
  (update tile :image (comp vec reverse)))

(defn fit-image [tile left-pred top-pred]
  (let [tile (loop [tile tile]
               (if (and (left-pred (left tile))
                        (some top-pred (map #(% tile) [top bottom])))
                 tile
                 (recur (rotate-image tile))))]
    (if (top-pred (top tile))
      tile
      (flip-image tile))))

(defn side-pred [side] #{side (reverse side)})

(defn stitch-image [tiles]
  (let [id->sides  (process-sides tiles)
        directory  (make-directory id->sides)
        first-tile (fit-image (tiles (first (find-corners id->sides directory)))
                              #(border? % directory)
                              #(border? % directory))]
    (loop [row [first-tile], image [], remaining-tiles (disj (set (keys tiles))
                                                             (:id first-tile))]
      (if (empty? remaining-tiles)
        (conj image row)
        (let [current-tile (last row)]
          (if (some-> current-tile right (border? directory))
            (recur [] (conj image row) remaining-tiles)
            (let [next-left (some-> current-tile right)
                  next-top  (some-> image last (nth (count row)) bottom)
                  next-tile (-> (or next-left next-top)
                                directory
                                (set/intersection remaining-tiles)
                                first
                                tiles)]
              (recur (conj row (apply fit-image
                                      next-tile
                                      (map (fn [side]
                                             (or (some-> side side-pred)
                                                 #(border? % directory)))
                                           [next-left next-top])))
                     image
                     (disj remaining-tiles (:id next-tile))))))))))

(defn borderless-image [{:keys [image]}]
  (map (fn [line] (subvec line 1 (dec (count line))))
       (subvec image 1 (dec (count image)))))

(def sea-monster
  ["                  #"
   "#    ##    ##    ###"
   " #  #  #  #  #  #"])

(defn sea-monster? [lines]
  (every? (fn [[sea-monster-line line]]
            (every? (fn [[sea-monster-char char]]
                      (or (= sea-monster-char \space) (= sea-monster-char char)))
                    (map vector sea-monster-line line)))
          (map vector sea-monster lines)))

(defn mark-sea-monster [sea-map [x y]]
  (reduce #(assoc-in %1 (cons :image %2) \O)
          sea-map
          (for [[x2 line] (map-indexed vector sea-monster)
                [y2 c]    (map-indexed vector line)
                :when     (= c \#)]
            [(+ x x2) (+ y y2)])))

(defn find-sea-monsters [sea-map]
  (some->> (for [[x lines]  (map-indexed vector (partition 3 1 (:image sea-map)))
                 [y window] (->> lines
                                 (map (partial partition 20 1))
                                 (apply map vector)
                                 (map-indexed vector))
                 :when      (sea-monster? window)]
             [x y])
           seq
           (reduce mark-sea-monster sea-map)))

(defn answer-part-2 [parsed-input]
  (let [sea-map {:image (vec (mapcat (fn [row]
                                       (apply mapv
                                              (comp vec concat)
                                              (map borderless-image row)))
                                     (stitch-image parsed-input)))}
        sea-map (some find-sea-monsters
                      (mapcat (fn [sea-map]
                                (take 4 (iterate rotate-image sea-map)))
                              [sea-map (flip-image sea-map)]))]
    (def my-sea-map sea-map)
    (count (filter #{\#} (flatten (:image sea-map))))))

(def part-2-answer (answer-part-2 parsed-input))

(comment
  part-2-answer
  ;; => 2155
  )
