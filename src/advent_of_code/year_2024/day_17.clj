(ns advent-of-code.year-2024.day-17
  (:require [advent-of-code.core :as core]
            [clojure.math :as math]
            [clojure.string :as string]
            [medley.core :as medley]))

(set! *warn-on-reflection* true)

(core/set-date! 2024 17)


;;;; Parse

(defn parse-input [input]
  {:registers (zipmap [:a :b :c] (map (comp parse-long  second)
                                      (re-seq #"Register .: (.+)" input)))
   :program   (mapv parse-long
                    (string/split (second (re-find #"Program: (.*)" input))
                                  #","))})

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn start-program [computer-state]
  (assoc computer-state :instruction-pointer 0, :output []))

(defmulti step (fn [{:keys [instruction-pointer program]}]
                 (get program instruction-pointer)))

(defn literal [{:keys [instruction-pointer program]}]
  (program (inc instruction-pointer)))

(defn combo [{:as computer-state :keys [registers]}]
  (let [operand (literal computer-state)]
    (case (int operand)
      (0 1 2 3) operand
      4         (:a registers)
      5         (:b registers)
      6         (:c registers))))

(defn increment-instruction-pointer [computer-state]
  (update computer-state :instruction-pointer + 2))

(defn dv [{:as computer-state, :keys [registers]}]
  (quot (registers :a) (long (math/pow 2 (combo computer-state)))))

(defmethod step :default [computer-state]
  (assoc computer-state :halted true))

(defmethod step 0 [computer-state]      ; adv
  (-> computer-state
      (assoc-in [:registers :a] (dv computer-state))
      increment-instruction-pointer))

(defmethod step 1 [computer-state]      ; bxl
  (-> computer-state
      (update-in [:registers :b] bit-xor (literal computer-state))
      increment-instruction-pointer))

(defmethod step 2 [computer-state]      ; bst
  (-> computer-state
      (assoc-in [:registers :b] (mod (combo computer-state) 8))
      increment-instruction-pointer))

(defmethod step 3 [{:as computer-state, :keys [registers]}] ; jnz
  (if (zero? (registers :a))
    (increment-instruction-pointer computer-state)
    (assoc computer-state :instruction-pointer (literal computer-state))))

(defmethod step 4 [{:as computer-state, :keys [registers]}] ; bxc
  (-> computer-state
      (assoc-in [:registers :b] (bit-xor (registers :b) (registers :c)))
      increment-instruction-pointer))

(defmethod step 5 [computer-state]      ; out
  (-> computer-state
      (update :output conj (mod (combo computer-state) 8))
      increment-instruction-pointer))

(defmethod step 6 [computer-state]      ; bdv
  (-> computer-state
      (assoc-in [:registers :b] (dv computer-state))
      increment-instruction-pointer))

(defmethod step 7 [computer-state]      ; cdv
  (-> computer-state
      (assoc-in [:registers :c] (dv computer-state))
      increment-instruction-pointer))

(defn outputs [computer-state]
  (lazy-seq
   (when-some [computer-state (->> computer-state
                                   (iterate step)
                                   (take-while (complement :halted))
                                   (medley/find-first (comp seq :output)))]
     (cons (get-in computer-state [:output 0])
           (outputs (assoc computer-state :output []))))))

(defn answer-part-1 [computer-state]
  (string/join "," (outputs (start-program computer-state))))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 "4,6,3,5,6,3,5,2,1,0"]
  [:puzzle "2,3,4,7,5,7,3,0,7"])


;;;; Part 2

(defn answer-part-2 [{:as computer-state, :keys [program]}]
  (let [computer-state (start-program computer-state)
        outputs        (fn [a]
                         (outputs (assoc-in computer-state [:registers :a] a)))]
    (->> program
         (iterate rest)
         (take-while seq)
         reverse
         (reduce (fn [as target]
                   (eduction (mapcat (fn [a]
                                       (let [a (bit-shift-left a 3)]
                                         (eduction (filter (comp #{target}
                                                                 outputs))
                                                   (range a (+ a 8))))))
                             as))
                 '(0))
         first)))

(core/set-answer-fn! 2 answer-part-2
  [:sample2 117440]
  [:puzzle 190384609508367])
