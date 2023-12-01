(ns advent-of-code.year-2021.day-01b
  (:require [advent-of-code.core :as core]
            [clojure.core.logic :as logic]
            [clojure.core.logic.fd :as fd]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(def input (core/get-input))

(defn parse-input [input] (map #(Long/parseLong %) (string/split-lines input)))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

(def max-reading (* (apply max parsed-input) 3))
(def max-count (dec (count parsed-input)))

(logic/defne increase-counto [l count]
  ([[_] _] (logic/== count 0))
  ([[a b . d] _] (logic/fresh [rest-count]
                   (increase-counto (logic/lcons b d) rest-count)
                   (fd/in a b (fd/interval max-reading))
                   (logic/conde
                     [(fd/< a b)
                      (fd/in rest-count count (fd/interval max-count))
                      (fd/+ rest-count 1 count)]
                     [(fd/>= a b) (logic/== count rest-count)]))))

(defn answer-part-1 [parsed-input]
  (first (logic/run 1 [answer]
           (increase-counto parsed-input answer))))

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 1400))


;;; Part 2
;;; ============================================================================

(logic/defne windowo [l out]
  ([[_] _] (logic/== out ()))
  ([[_ _] _] (logic/== out ()))
  ([[a b c . _] _] (logic/fresh [window d more-out]
                     (fd/in a b c (fd/interval max-reading))
                     (fd/in window (fd/interval (* max-reading 3)))
                     (fd/eq (= (+ a b c) window))
                     (logic/resto l d)
                     (windowo d more-out)
                     (logic/== out (logic/lcons window more-out)))))

(defn answer-part-2 [parsed-input]
  (first (logic/run 1 [answer]
           (logic/fresh [l]
             (windowo parsed-input l)
             (increase-counto l answer)))))

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer 1429))
