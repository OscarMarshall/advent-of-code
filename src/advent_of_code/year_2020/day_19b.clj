(ns advent-of-code.year-2020.day-19b
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2020 19)


;;;; Parse

(defn parse-input [input]
  (let [[rules messages] (map string/split-lines (string/split input #"\n\n"))]
    {:rules    (into {}
                     (map (comp (juxt (comp parse-long first) second)
                                #(string/split % #": ")))
                     rules)
     :messages messages}))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn rule->regex [rule rules]
  (if (int? rule)
    (recur (rules rule) rules)
    (condp re-matches rule
      #"\"(.*)\""     :>> (fn [[_ c]] c)
      #"(.*) \| (.*)" :>> (fn [[_ left right]]
                           (format "(?:%s|%s)"
                                   (rule->regex left rules)
                                   (rule->regex right rules)))
      (let [sub-rules (map parse-long (re-seq #"\d+" rule))]
        (string/join (map #(rule->regex % rules) sub-rules))))))

(defn answer-part-1 [parsed-input]
  (let [{:keys [rules messages]} parsed-input]
    (count (filter (partial re-matches (re-pattern (rule->regex 0 rules)))
                   messages))))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 147])


;;;; Part 2

(defn answer-part-2 [parsed-input]
  (let [{:keys [rules messages]} parsed-input
        re42                     (re-pattern (rule->regex 42 rules))
        re42+                    (re-pattern (format "^(?:%s)+"
                                                     (rule->regex 42 rules)))
        re31                     (re-pattern (rule->regex 31 rules))
        re31+                    (re-pattern (format "^(?:%s)+"
                                                     (rule->regex 31 rules)))]
    (count (filter (fn [s]
                     (when-let [re42+match (re-find re42+ s)]
                       (let [re42s (re-seq re42 re42+match)
                             s     (subs s (count re42+match))
                             re31s (some->> s
                                            (re-matches re31+)
                                            (re-seq re31))]
                         (when re31s
                           (> (count re42s) (count re31s))))))
                   messages))))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 263])
