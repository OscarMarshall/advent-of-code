(ns advent-of-code.year-2023.day-08
  (:require [advent-of-code.core :as core]
            [clojure.string :as str]
            [medley.core :as medley]))

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

(defn answer-part-1 [{:keys [instructions nodes]}]
  (->> (reductions (fn [id instruction]
                     ((case instruction \L :left, \R :right) (nodes id)))
                   "AAA"
                   (cycle instructions))
       (take-while (complement #{"ZZZ"}))
       count))

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

(defn answer-part-2 [x]
  x)

(println "### Sample Answer")

(def part-2-sample-answer (time (answer-part-2 sample-parsed-input)))

(prn part-2-sample-answer)
(println)

(println "### Answer")

(def part-2-answer (time (answer-part-2 parsed-input)))

(prn part-2-answer)
(println)

(assert (= part-2-sample-answer))
(assert (= part-2-answer part-2-answer))
