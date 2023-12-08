(ns advent-of-code.year-2023.day-08
  (:require [advent-of-code.core :as core]
            [clojure.string :as str]
            [medley.core :as medley]
            [clojure.math :as math]))

(println "# Day " 8)

(set! *warn-on-reflection* true)

(defn parse-nodes [nodes]
  (->> nodes
       str/split-lines
       (map #(zipmap [:id :left :right]
                     (rest (re-matches #"(...) = \((...), (...)\)" %))))
       (medley/index-by :id)))

(defn parse-input [input]
  (let [[instructions nodes] (str/split input #"\n\n")]
    {:instructions instructions
     :nodes (parse-nodes nodes)}))

(def sample-input (core/get-input *file* :sample))
(def sample-parsed-input (parse-input sample-input))

(def input (core/get-input *file*))
(def parsed-input (parse-input input))


;;;; Part 1

(println "## Part 1")

(defn answer [{:keys [instructions nodes]} start ends]
  (->> (reductions (fn [id instruction]
                     ((case instruction \L :left, \R :right) (nodes id)))
                   start
                   (cycle instructions))
       (take-while (complement ends))
       count))

(defn answer-part-1 [network] (answer network "AAA" #{"ZZZ"}))

(println "### Sample Answer")

(def part-1-sample-answer (time (answer-part-1 sample-parsed-input)))

(prn part-1-sample-answer)
(println)

(println "### Answer")

(def part-1-answer (time (answer-part-1 parsed-input)))

(prn part-1-answer)
(println)

(assert (= part-1-sample-answer 6))
(assert (= part-1-answer 21409))


;;;; Part 2

(println "## Part 2")

(defn starting-ids [ids]
  (filter #(re-matches #"..A" %) ids))

(defn ending-ids [ids]
  (set (filter #(re-matches #"..Z" %) ids)))

(defn factors
  ([n] (factors n 2))
  ([n last-checked]
   (if-some [factor (first (filter #(zero? (rem n %))
                                   (range last-checked (math/sqrt n))))]
     (cons factor (factors (/ n factor) factor))
     [n])))

(defn answer-part-2 [{:as network, :keys [nodes]}]
  (let [ending-ids (ending-ids (keys nodes))]
    (->> nodes
         keys
         starting-ids
         (map #(frequencies (factors (answer network % ending-ids))))
         (apply merge-with max)
         (mapcat (fn [[x n]] (repeat n x)))
         (apply *))))

(comment
  (starting-ids (keys (:nodes parsed-input)))
  )

(println "### Sample Answer")

(def sample2
  "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)")

(def part-2-sample-answer (time (answer-part-2 (parse-input sample2))))

(prn part-2-sample-answer 6)
(println)

(println "### Answer")

(def part-2-answer (time (answer-part-2 parsed-input)))

(prn part-2-answer)
(println)

(assert (= part-2-sample-answer))
(assert (= part-2-answer 21165830176709))
