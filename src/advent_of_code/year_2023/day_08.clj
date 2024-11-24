(ns advent-of-code.year-2023.day-08
  (:require [advent-of-code.core :as core]
            [clojure.string :as str]
            [medley.core :as medley]))

(set! *warn-on-reflection* true)

(core/set-date! 2023 8)


;;;; Parse

(defn parse-nodes [nodes]
  (->> nodes
       str/split-lines
       (map #(zipmap [:id :left :right]
                     (rest (re-matches #"(...) = \((...), (...)\)" %))))
       (medley/index-by :id)))

(defn parse-input [input]
  (let [[instructions nodes] (str/split input #"\n\n")]
    {:instructions (map {\L :left, \R :right} instructions)
     :nodes (parse-nodes nodes)}))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn answer [{:keys [instructions nodes]} start ends]
  (->> instructions
       cycle
       (reductions (fn [id instruction] (instruction (nodes id))) start)
       (take-while #(not (ends %)))
       count))

(defn answer-part-1 [network] (answer network "AAA" #{"ZZZ"}))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 2]
  [:sample2 6]
  [:puzzle 21409])


;;;; Part 2

(defn starting-ids [ids]
  (filter #(re-matches #"..A" %) ids))

(defn ending-ids [ids]
  (set (filter #(re-matches #"..Z" %) ids)))

(defn gcd [x y]
  (cond
    (< x y) (recur x (- y x))
    (> x y) (recur (- x y) y)
    :else   x))

(defn lcm [x y]
  (/ (* x y) (gcd x y)))

(defn answer-part-2 [{:as network, :keys [nodes]}]
  (let [ending-ids (ending-ids (keys nodes))]
    (transduce (map #(answer network % ending-ids))
               (completing lcm)
               1
               (starting-ids (keys nodes)))))

(core/set-answer-fn! 2 answer-part-2
  [:sample3 6]
  [:puzzle 21165830176709])
