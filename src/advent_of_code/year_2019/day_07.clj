(ns advent-of-code.year-2019.day-07
  (:require [advent-of-code.core :as core]
            [advent-of-code.year-2019.intcode-computer :as intcode-computer]
            [clojure.math.combinatorics :as combo]
            [medley.core :as medley]))

(set! *warn-on-reflection* true)

(core/set-date! 2019 7)


;;;; Parse

(core/set-parse-fn! intcode-computer/parse-program)


;;;; Part 1

(defn start-amplifiers [program phase-settings]
  (into (medley/queue)
        (map (partial intcode-computer/start program))
        phase-settings))

(defn run-amplifiers [amplifiers input]
  (let [{:as amplifier, :keys [interrupt]} (peek amplifiers)]
    (if (= interrupt :halt)
      input
      (let [[output amplifier] (-> amplifier
                                   (intcode-computer/continue input)
                                   intcode-computer/read-output)]
        (recur (conj (pop amplifiers) amplifier) output)))))

(defn max-signal [program phase-settings]
  (transduce (comp (map (partial start-amplifiers program))
                   (map #(run-amplifiers % 0)))
             max
             0
             (combo/permutations phase-settings)))

(defn answer-part-1 [program]
  (max-signal program (range 5)))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 43210]
  [:sample2 54321]
  [:sample3 65210]
  [:puzzle 45730])


;;;; Part 2

(defn answer-part-2 [program]
  (max-signal program (range 5 10)))

(core/set-answer-fn! 2 answer-part-2
  [:sample4 139629729]
  [:sample5 18216]
  [:puzzle 5406484])
