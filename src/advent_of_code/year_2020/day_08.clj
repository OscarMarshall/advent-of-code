(ns advent-of-code.year-2020.day-08
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2020 8)


;;;; Parse

(defn parse-input [input]
  (mapv (fn [instruction]
          (let [[op arg] (string/split instruction #" ")]
            {:op (keyword op), :arg (parse-long arg)}))
        (string/split-lines input)))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn next-instruction [state] (update state :ip inc))

(defn step [{:as state, :keys [ip prog]}]
  (let [{:keys [op arg]} (nth prog ip)]
    (case op
      :acc (-> state (update :acc + arg) next-instruction)
      :jmp (update state :ip + arg)
      :nop (next-instruction state)
      nil  state)))

(defn initialize-prog [prog]
  {:acc 0, :ip 0, :prog prog})

(defn run-prog [prog]
  (iterate step (initialize-prog prog)))

(defn answer-part-1 [parsed-input]
  (reduce (fn [[seen previous-acc] {:keys [acc ip]}]
            (if (seen ip) (reduced previous-acc) [(conj seen ip) acc]))
          [#{} 0]
          (run-prog parsed-input)))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 1489])


;;;; Part 2

(defn check-solution [prog]
  (reduce (fn [[seen previous-acc] {:keys [acc ip]}]
            (cond
              (seen ip)           (reduced false)
              (= ip (count prog)) (reduced previous-acc)
              :else               [(conj seen ip) acc]))
          [#{} 0]
          (run-prog prog)))

(defn answer-part-2 [parsed-input]
  (some (fn [i]
          (let [{:keys [op]} (nth parsed-input i)]
            (case op
              :jmp (check-solution (assoc-in parsed-input [i :op] :nop))
              :nop (check-solution (assoc-in parsed-input [i :op] :jmp))
              false)))
        (range (count parsed-input))))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 1539])
