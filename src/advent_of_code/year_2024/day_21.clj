(ns advent-of-code.year-2024.day-21
  (:require [advent-of-code.core :as core]
            [advent-of-code.grid :as grid]
            [advent-of-code.utils :as utils]
            [clojure.string :as string]
            [medley.core :as medley]
            [clojure.math.combinatorics :as combo]))

(set! *warn-on-reflection* true)

(core/set-date! 2024 21)


;;;; Parse

(defn parse-input [input] (string/split-lines input))

(core/set-parse-fn! parse-input)


;;;; Part 1

(def numeric-keypad
  (grid/parse (str "789\n"
                   "456\n"
                   "123\n"
                   " 0A")
              (fn [c] (when (not= c \space) c))))

(def directional-keypad
  (grid/parse (str " ^A\n"
                   "<v>")
              (fn [c] (when (not= c \space) c))))

(def paths-between-buttons
  (memoize
   (fn [{:keys [pois]} start end]
     (->>
      start
      (utils/djkstra-walk
       (fn [coordinates]
         (sequence (comp (map (fn [direction coordinates]
                                (vary-meta coordinates
                                           assoc
                                           ::utils/edge direction)))
                         (filter pois))
                   [\^ \> \v \<]
                   (grid/neighbors coordinates))))
      (some (fn [[coordinates _ paths]]
              (when (= coordinates end)
                (sequence (comp (map (partial mapv
                                              (comp (some-fn identity
                                                             (constantly \A))
                                                    ::utils/edge
                                                    meta)))
                                (filter (fn [path]
                                          (<= (count (partition-by identity
                                                                   path))
                                              3))))
                          paths))))))))

#_(def proxy-code
  (memoize
   (fn [[{:as keypad, :keys [indices]} & keypads] code]
     (if keypad
       (let [coordinates (map (comp first indices) (cons \A code))]
         (recur keypads
                (mapcat (fn [a b]
                          (apply min
                                 (partial proxy-code keypads)
                                 (paths-between-buttons keypad a b)))
                        coordinates
                        (rest coordinates))))
       code))))

(def proxy-code
  (memoize
   (fn [[{:as keypad, :keys [indices]} & keypads] code]
     (if keypad
       (let [coordinates (map (comp first indices) (cons \A code))]
         (reduce +
                 (map (comp (partial apply min)
                            (partial map (partial proxy-code keypads))
                            (partial paths-between-buttons keypad))
                      coordinates
                      (rest coordinates))))
       (count code)))))

(defn code-complexity [code proxies]
  (* (proxy-code (cons numeric-keypad (repeat proxies directional-keypad)) code)
     (parse-long (nth (re-matches #"0*(\d+)A" code) 1))))

(defn answer-part-1 [codes]
  (transduce (map #(code-complexity % 2)) + codes))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 126384]
  [:puzzle 163920])


;;;; Part 2

(defn answer-part-2 [codes]
  (transduce (map #(code-complexity % 25)) + codes))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle [< 239459990642564] 204040805018350])
