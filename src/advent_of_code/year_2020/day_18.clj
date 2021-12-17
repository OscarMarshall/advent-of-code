(ns advent-of-code.year-2020.day-18
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(def input (core/get-input))

(defn parse-input [input]
  (map (fn [line]
         (map (fn [[token number]]
                (or (some-> number Long/parseLong)
                    (case token
                      "+" +
                      "*" *
                      "(" \(
                      ")" \))))
              (re-seq #"(\d+)|\+|\*|\(|\)" line)))
       (string/split-lines input)))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

(defn apply-parens [tokens]
  (loop [[token & tokens] tokens, result []]
    (case token
      nil (seq result)
      \(  (let [[tokens sub-result] (apply-parens tokens)]
            (recur tokens (conj result sub-result)))
      \)  [tokens (seq result)]
      (recur tokens (conj result token)))))

(defn eval-expr [expr]
  (if (seq? expr)
    (let [[a op b & more] expr]
      (if op
        (recur (cons (op (eval-expr a) (eval-expr b)) more))
        a))
    expr))

(defn answer-part-1 [parsed-input]
  (apply + (map (comp eval-expr apply-parens) parsed-input)))

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 45283905029161))


;;; Part 2
;;; ============================================================================

(defn apply-precedence [expr]
  (if (seq? expr)
    (loop [result [], [a op b & expr] (cons (apply-precedence (first expr))
                                            (rest expr))]
      (cond
        (nil? op) (if (empty? result) a (seq (conj result a)))
        (= op *)  (recur (conj result a op) (cons (apply-precedence b) expr))
        (= op +)  (recur result (cons `(~a ~op ~(apply-precedence b)) expr))))
    expr))

(defn answer-part-2 [parsed-input]
  (apply + (map (comp eval-expr apply-precedence apply-parens) parsed-input)))

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer 216975281211165))
