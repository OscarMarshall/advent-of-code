(ns advent-of-code.year-2025.day-02
  (:require
   [advent-of-code.core :as core]
   [clojure.math :as math]
   [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2025 2)


;;;; Parse

(defn parse-input [input]
  (map (fn [s]
         (let [[x y] (string/split s #"-")] [(parse-long x) (parse-long y)]))
       (string/split (string/trim input) #",")))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn invalid-ids [[a b]]
  (let [sa (str a)
        n (count sa)
        m (quot n 2)
        x (if (odd? n)
            (long (math/pow 10 m))
            (parse-long (subs sa 0 m)))]
    (->> (range x ##Inf)
         (map (fn [x] (parse-long (str x x))))
         (drop-while #(< % a))
         (take-while #(<= % b)))))

(defn invalid-ids* [[a b]]
  (filter #(re-matches #"(.+)\1" (str %)) (range a (inc b))))

(defn answer-part-1 [xs] (transduce (mapcat invalid-ids) + xs))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 1227775554]
  [:puzzle [not= 18502591410] 23701357374])


;;;; Part 2

(defn invalid-ids2 [[a b]]
  (into #{}
        (mapcat (fn [x]
                  (->> (range)
                       (drop 2)
                       (map (fn [n] (parse-long (apply str (repeat n x)))))
                       (drop-while #(< % a))
                       (take-while #(<= % b)))))
        (range 1 (math/pow 10 (quot (count (str b)) 2)))))

(defn invalid-ids2* [[a b]]
  (filter #(re-matches #"(.+)\1+" (str %)) (range a (inc b))))

(defn answer-part-2 [xs]
  (transduce (mapcat (fn [x] (invalid-ids2 x))) + xs))

(core/set-answer-fn! 2 answer-part-2
    [:sample1 4174379265]
    [:puzzle [< 34284458980] 34284458938])
