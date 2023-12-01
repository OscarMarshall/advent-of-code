(ns advent-of-code.year-2020.day-19b
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(def input (core/get-input))

(defn parse-input [input]
  (let [[rules messages] (map string/split-lines (string/split input #"\n\n"))]
    {:rules    (into {}
                     (map (comp (juxt (comp #(Long/parseLong %) first)
                                      second)
                                #(string/split % #": ")))
                     rules)
     :messages messages}))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

(defn rule->regex [rule rules]
  (if (int? rule)
    (recur (rules rule) rules)
    (condp re-matches rule
      #"\"(.*)\""     :>> (fn [[_ c]] c)
      #"(.*) \| (.*)" :>> (fn [[_ left right]]
                           (format "(?:%s|%s)"
                                   (rule->regex left rules)
                                   (rule->regex right rules)))
      (let [sub-rules (map #(Long/parseLong %) (re-seq #"\d+" rule))]
        (string/join (map #(rule->regex % rules) sub-rules))))))

(defn answer-part-1 [parsed-input]
  (let [{:keys [rules messages]} parsed-input]
    (count (filter (partial re-matches (re-pattern (rule->regex 0 rules)))
                   messages))))

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 147))


;;; Part 2
;;; ============================================================================

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

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer 263))
