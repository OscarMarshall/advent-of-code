(ns advent-of-code-2020.day-08
  (:require [advent-of-code-2020.core :as core]
            [clojure.string :as string]))

;;; Part 1
;;; ============================================================================

(def input (core/get-input))

(defn parse-input [input]
  (mapv (fn [instruction]
          (let [[op arg] (string/split instruction #" ")]
            {:op (keyword op), :arg (Long/parseLong arg)}))
        (string/split-lines input)))

(def parsed-input (parse-input input))

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

(def part-1-answer (answer-part-1 parsed-input))

(comment
  part-1-answer
  ;; => 1489
  )

;;; Part 2
;;; ============================================================================

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

(def part-2-answer (answer-part-2 parsed-input))

(comment
  part-2-answer
  ;; => 1539
  )
