(ns advent-of-code.year-2023.day-19
  (:require [advent-of-code.core :as core]
            [clojure.string :as str]))

(println "# Day 19")

(set! *warn-on-reflection* true)

(def op->fn {"<" <, ">" >})

(defn parse-rule [rule]
  (condp re-matches rule
    #"([xmas])([<>])(\d+):([A-z]+)"
    :>> (fn [[_ & groups]]
          (mapv #(%1 %2) [keyword op->fn parse-long keyword] groups))

    #"[A-z]+" :>> keyword))

(defn parse-workflow [s]
  (let [[_ workflow-name rules] (re-matches #"(.*)\{(.*)\}" s)]
    [(keyword workflow-name) (mapv parse-rule (str/split rules #","))]))

(defn parse-part [s]
  (zipmap [:x :m :a :s]
          (map parse-long
               (rest (re-matches #"\{x=(\d+),m=(\d+),a=(\d+),s=(\d+)\}" s)))))

(defn parse-input [input]
  (let [[workflows parts] (str/split input #"\n\n")]
    {:workflows (into {} (map parse-workflow) (str/split-lines workflows))
     :parts     (map parse-part (str/split-lines parts))}))

;;;; Part 1

(defn accepted? [part workflows]
  (loop [workflow-name :in]
    (case workflow-name
      :A true
      :R false
      (let [workflow (workflows workflow-name)]
        (recur (or (some (fn [[category op rhs dest]]
                           (when (op (category part) rhs) dest))
                         (butlast workflow))
                   (peek workflow)))))))

(defn answer-part-1 [{:keys [workflows parts]}]
  (transduce (comp (filter #(accepted? % workflows)) (mapcat vals)) + parts))

(core/part 1
  parse-input answer-part-1 *file*
  [:sample1 19114]
  [:input 425811])


;;;; Part 2

(defn answer-part-2 [x]
  x)

(core/part 2
  parse-input answer-part-2 *file*
  [:sample1 #_?]
  [:input #_(core/current-answer 2)])
