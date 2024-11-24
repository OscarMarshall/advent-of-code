(ns advent-of-code.year-2023.day-19
  (:require [advent-of-code.core :as core]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)

(core/set-date! 2023 19)


;;;; Parse

(defn parse-rule [rule]
  (condp re-matches rule
    #"([xmas])([<>])(\d+):([A-z]+)"
    :>> (fn [[_ & groups]]
          (mapv #(%1 %2) [keyword keyword parse-long keyword] groups))

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

(core/set-parse-fn! parse-input)

;;;; Part 1

(def op->fn {:< <, :> >})

(defn accepted? [part workflows]
  (loop [workflow-name :in]
    (case workflow-name
      :A true
      :R false
      (let [workflow (workflows workflow-name)]
        (recur (or (some (fn [[rating op rhs dest]]
                           (when ((op->fn op) (rating part) rhs) dest))
                         (butlast workflow))
                   (peek workflow)))))))

(defn answer-part-1 [{:keys [workflows parts]}]
  (transduce (comp (filter #(accepted? % workflows)) (mapcat vals)) + parts))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 19114]
  [:puzzle 425811])


;;;; Part 2

(defn combinations
  ([workflows]
   (combinations :in (zipmap [:x :m :a :s] (repeat [1 4000])) workflows))
  ([workflow-name possible-ratings workflows]
   (case workflow-name
     :A (transduce (map (fn [[a b]] (inc (- b a)))) * (vals possible-ratings))
     :R 0
     (second (reduce (fn [[possible-ratings acc] rule]
                       (if (keyword? rule)
                         [possible-ratings
                          (+ acc
                             (combinations rule possible-ratings workflows))]
                         (let [[rating op rhs dest] rule
                               [low high]           (rating possible-ratings)]
                           (case op
                             :< (cond
                                  (< high rhs)
                                  (reduced [possible-ratings
                                            (+ acc
                                               (combinations dest
                                                             possible-ratings
                                                             workflows))])

                                  (< low rhs)
                                  [(assoc-in possible-ratings [rating 0] rhs)
                                   (+ acc
                                      (combinations dest
                                                    (assoc-in possible-ratings
                                                              [rating 1]
                                                              (dec rhs))
                                                    workflows))]

                                  :else
                                  [possible-ratings acc])
                             :> (cond
                                  (> low rhs)
                                  (reduced [possible-ratings
                                            (+ acc
                                               (combinations dest
                                                             possible-ratings
                                                             workflows))])

                                  (> high rhs)
                                  [(assoc-in possible-ratings [rating 1] rhs)
                                   (+ acc
                                      (combinations dest
                                                    (assoc-in possible-ratings
                                                              [rating 0]
                                                              (inc rhs))
                                                    workflows))]

                                  :else
                                  [possible-ratings acc])))))
                     [possible-ratings 0]
                     (workflows workflow-name))))))

(defn answer-part-2 [{:keys [workflows]}]
  (combinations workflows))

(core/set-answer-fn! 2 answer-part-2
  [:sample1 167409079868000]
  [:puzzle 131796824371749])
