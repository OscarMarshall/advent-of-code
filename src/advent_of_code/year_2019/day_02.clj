(ns advent-of-code.year-2019.day-02
  (:require [advent-of-code.core :as core]
            [advent-of-code.year-2019.intcode-computer :as intcode-computer]
            [clojure.math.combinatorics :as combo]
            [medley.core :as medley]))

(set! *warn-on-reflection* true)

(core/set-date! 2019 2)


;;;; Parse

(core/set-parse-fn! intcode-computer/parse-program)


;;;; Part 1

(defn run-gravity-assist-program [program noun verb]
  (get-in (intcode-computer/start (assoc program 1 noun, 2 verb)) [:memory 0]))

(defn answer-part-1 [program]
  (run-gravity-assist-program program 12 2))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 3654868])


;;;; Part 2

(defn answer-part-2 [program]
  (let [[noun verb] (medley/find-first (fn [[noun verb]]
                                         (= (run-gravity-assist-program program
                                                                        noun
                                                                        verb)
                                            19690720))
                                       (combo/cartesian-product (range 100)
                                                                (range 100)))]
    (+ (* 100 noun) verb)))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 7014])
