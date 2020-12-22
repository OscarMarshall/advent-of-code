(ns advent-of-code-2020.day-21
  (:require [advent-of-code-2020.core :as core]
            [clojure.set :as set]
            [clojure.string :as string]))

;;; Part 1
;;; ============================================================================

(def input (core/get-input))

(defn parse-input [input]
  (map (fn [line]
         (let [[_ ingredients allergens] (re-matches #"(.*) \(contains (.*)\)"
                                                     line)]
           [(set (string/split ingredients #" "))
            (set (string/split allergens #", "))]))
       (string/split-lines input)))

(def parsed-input (parse-input input))

(defn find-allergen-ingredients [foods]
  (let [all-allergens (apply set/union (map second foods))]
    (loop [undetermined-allergens (into clojure.lang.PersistentQueue/EMPTY
                                        all-allergens)
           allergen-ingredients   {}]
      (if (empty? undetermined-allergens)
        allergen-ingredients
        (let [allergen (peek undetermined-allergens)

              possible-ingredients
              (-> foods
                  (->> (filter (fn [[_ allergens]]
                                 (contains? allergens
                                            allergen)))
                       (map first)
                       (apply set/intersection))
                  (set/difference (set (vals allergen-ingredients))))]
          (if (= (count possible-ingredients) 1)
            (recur (pop undetermined-allergens)
                   (assoc allergen-ingredients allergen
                          (first possible-ingredients)))
            (recur (conj (pop undetermined-allergens)
                         (peek undetermined-allergens))
                   allergen-ingredients)))))))

(defn answer-part-1 [parsed-input]
  (let [allergen-ingredients (find-allergen-ingredients parsed-input)]
    (->> parsed-input
         (mapcat (comp seq first))
         (remove (set (vals allergen-ingredients)))
         count)))

(def part-1-answer (answer-part-1 parsed-input))

(comment
  part-1-answer
  ;; => 2584
  )

;;; Part 2
;;; ============================================================================

(defn answer-part-2 [parsed-input]
  (let [allergen-ingredients (find-allergen-ingredients parsed-input)]
    (->> allergen-ingredients
         (sort-by key)
         (map val)
         (string/join ","))))

(def part-2-answer (answer-part-2 parsed-input))

(comment
  part-2-answer
  ;; => "fqhpsl,zxncg,clzpsl,zbbnj,jkgbvlxh,dzqc,ppj,glzb"
  )
