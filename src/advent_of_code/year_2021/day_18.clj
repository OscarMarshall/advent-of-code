(ns advent-of-code.year-2021.day-18
  (:require [advent-of-code.core :as core]
            [clojure.edn :as edn]
            [clojure.string :as string]))

(def input (core/get-input))

(defn parse-input [input] (map edn/read-string (string/split-lines input)))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

(def loc-seq
  (memoize
   (fn [snail-number]
     (cond-> '(())
       (vector? snail-number) (concat
                               (apply
                                concat
                                (map-indexed (fn [i snail-number]
                                               (map (partial cons i)
                                                    (loc-seq snail-number)))
                                             snail-number)))))))

(defn explode? [snail-number loc]
  (and (vector? (get-in snail-number loc)) (= (count loc) 4)))

(defn explode-snail-number [snail-number loc]
  (let [[left right] (get-in snail-number loc)
        locs         (into []
                           (filter (comp integer?
                                         (partial get-in snail-number)))
                           (loc-seq snail-number))
        loc-i        (some (fn [[i x]] (when (= (butlast x) loc) i))
                           (map-indexed vector locs))
        left-loc     (get locs (dec loc-i))
        right-loc    (get locs (+ loc-i 2))]
    (-> snail-number
        (assoc-in loc 0)
        (cond-> left-loc (update-in left-loc + left)
                right-loc (update-in right-loc + right)))))

(def split? (comp (every-pred integer? (partial < 9)) (partial get-in)))

(defn split-snail-number [snail-number loc]
  (update-in snail-number loc (comp (partial mapv long)
                                    (juxt #(Math/floor %) #(Math/ceil %))
                                    #(/ % 2))))

(defn find-loc [pred snail-number]
  (first (filter (partial pred snail-number) (loc-seq snail-number))))

(defn reduce-snail-number [snail-number]
  (let [reduced-1 (condp find-loc snail-number
                    explode? :>> (partial explode-snail-number snail-number)
                    split?   :>> (partial split-snail-number snail-number)
                    nil)]
    (if (nil? reduced-1)
      snail-number
      (recur reduced-1))))

(defn add-snail-numbers [x y]
  (reduce-snail-number [x y]))

(defn magnitude [snail-number]
  (if (integer? snail-number)
    snail-number
    (apply + (map * '(3 2) (map magnitude snail-number)))))

(defn answer-part-1 [parsed-input]
  (magnitude (reduce add-snail-numbers parsed-input)))

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 3987))


;;; Part 2
;;; ============================================================================

(defn answer-part-2 [parsed-input]
  (reduce max (for [x parsed-input, y parsed-input, :when (not= x y)]
                (magnitude (add-snail-numbers x y)))))

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer 4500))
