(ns advent-of-code.year-2020.day-21
  (:require [advent-of-code.core :as core]
            [clojure.set :as set]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2020 21)


;;;; Parse

(defn parse-input [input]
  (map (fn [line]
         (let [[_ ingredients allergens] (re-matches #"(.*) \(contains (.*)\)"
                                                     line)]
           [(set (string/split ingredients #" "))
            (set (string/split allergens #", "))]))
       (string/split-lines input)))

(core/set-parse-fn! parse-input)


;;;; Part 1

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

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 2584])


;;;; Part 2

(defn answer-part-2 [parsed-input]
  (let [allergen-ingredients (find-allergen-ingredients parsed-input)]
    (->> allergen-ingredients
         (sort-by key)
         (map val)
         (string/join ","))))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle "fqhpsl,zxncg,clzpsl,zbbnj,jkgbvlxh,dzqc,ppj,glzb"])
