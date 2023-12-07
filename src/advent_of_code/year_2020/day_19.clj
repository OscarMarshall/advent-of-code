(ns advent-of-code.year-2020.day-19
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(def input (core/get-input *file*))

(defn empty-matcher [] {:type :empty-matcher})
(defn empty-matcher? [x] (boolean (some-> x :type (= :empty-matcher))))
(defn char-matcher [c] {:type :char-matcher, :value c})
(defn cat-matcher [& xs] {:type :cat-matcher, :value xs})
(defn cat-matcher? [x] (boolean (some-> x :type (= :cat-matcher))))
(defn or-matcher [& xs] {:type :or-matcher, :value xs})
(defn or-matcher? [x] (boolean (some-> x :type (= :or-matcher))))

(defn make-matcher [definition]
  (condp re-matches definition
    #"\"(.)\""      :>> (fn [[_ character]] (char-matcher (first character)))
    #"\d+(?: \d+)*" :>> (fn [xs]
                          (apply cat-matcher (map #(Long/parseLong %)
                                                  (string/split xs #" "))))
    #"(.*) \| (.*)" :>> (fn [[_ & a+b]]
                          (apply or-matcher (map make-matcher a+b)))))
(defn parse-input [input]
  (let [[rules messages] (map string/split-lines (string/split input #"\n\n"))]
    {:rules    (into {}
                     (map (comp (juxt (comp #(Long/parseLong %) first)
                                      (comp make-matcher second))
                                #(string/split % #": ")))
                     rules)
     :messages messages}))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

(defn matched? [{:keys [type value]}]
  (case type
    :empty-matcher true
    :char-matcher  false
    :cat-matcher   (every? matched? value)
    :or-matcher    (some matched? value)
    false))

(defn next-char [{:keys [type value], :as matcher}]
  (case type
    :empty-matcher false
    :char-matcher  value
    :cat-matcher   (next-char (first value))
    :or-matcher    (#{(next-char (first value))} (next-char (second value)))
    matcher))

(def match
  (memoize
   (fn [{:keys [type value], :as matcher} c rules]
     (case type
       :empty-matcher nil
       :char-matcher  (when (= c value) (empty-matcher))
       :cat-matcher   (when-let [sub-matcher (match (first value) c rules)]
                        (let [new-value (cond->> (rest value)
                                          (not (empty-matcher? sub-matcher))
                                          (cons sub-matcher))]
                          (cond-> (case (count new-value)
                                    0 (empty-matcher)
                                    1 (first new-value)
                                    (if (cat-matcher? sub-matcher)
                                      (update (first new-value)
                                              :value concat (rest new-value))
                                      (apply cat-matcher new-value)))
                            (matched? (first value))
                            (or-matcher (match (update matcher :value rest)
                                               c
                                               rules)))))
       :or-matcher    (let [[a b] value
                            a     (match a c rules)
                            b     (match b c rules)]
                        (if (and a b)
                          (loop [prefix  []
                                 matcher (or-matcher a b)]
                            (if-let [c (next-char matcher)]
                              (recur (conj prefix c) (match matcher c rules))
                              (if (seq prefix)
                                (apply cat-matcher (seq (conj prefix matcher)))
                                matcher)))
                          (or a b)))
       (when matcher
         (if (= matcher c)
           (empty-matcher)
           (match (rules matcher) c rules)))))))

(defn matcher-str [{:keys [type value], :as matcher}]
  (case type
    :empty-matcher "\"\""
    :char-matcher  (format "\"%s\"" value)
    :cat-matcher   (string/join " " (map matcher-str value))
    :or-matcher    (format "(%s)" (string/join " | " (map matcher-str value)))
    (str matcher)))

(defn answer-part-1 [parsed-input]
  (let [{:keys [rules messages]} parsed-input]
    (->> messages
         (filter (fn [message]
                   (some-> (reduce (fn [matcher c]
                                     (or (match matcher c rules) (reduced nil)))
                                   0
                                   message)
                           matched?)))
         count)))

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 147))


;;; Part 2
;;; ============================================================================

(defn answer-part-2 [parsed-input]
  (-> parsed-input
      (assoc-in [:rules 8] (make-matcher "42 | 42 8"))
      (assoc-in [:rules 11] (make-matcher "42 31 | 42 11 31"))
      answer-part-1))

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer 263))
