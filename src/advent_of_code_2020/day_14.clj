(ns advent-of-code-2020.day-14
  (:require [advent-of-code-2020.core :as core]
            [clojure.string :as string]))

;;; Part 1
;;; ============================================================================

(def input (core/get-input))

(defn parse-input [input]
  (map (fn [line]
         (if-let [[_ address value] (re-matches #"mem\[(\d+)\] = (\d+)" line)]
           {:op      :set-mem
            :address (Long/parseLong address)
            :value   (Long/parseLong value)}
           (let [[_ mask] (re-matches #"mask = (.*)" line)]
             {:op :set-mask, :value mask})))
       (string/split-lines input)))

(def parsed-input (parse-input input))

(defn apply-mask [number mask]
  (let [base-2 (Long/toString number 2)]
    (Long/parseLong (apply str
                           (map #(or (#{\0 \1} %1) %2)
                                mask
                                (apply str
                                       (concat (repeat (- 36 (count base-2)) \0)
                                               base-2))))
                    2)))

(defn answer-part-1 [parsed-input]
  (->>
   parsed-input
   (reduce (fn [[mem mask] {:as command :keys [op value]}]
             (case op
               :set-mask [mem value]
               :set-mem  [(assoc mem (:address command) (apply-mask value mask))
                          mask]))
           [{} "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"])
   first
   vals
   (apply +)))

(def part-1-answer (answer-part-1 parsed-input))

(comment
  part-1-answer
  ;; => 15403588588538
  )

;;; Part 2
;;; ============================================================================

(defn apply-mask2 [number mask]
  (let [base-2 (Long/toString number 2)]
    (->> (map #(or (#{\1 \X} %1) %2)
              mask
              (apply str
                     (concat (repeat (- 36 (count base-2)) \0)
                             base-2)))
         (reduce (fn [possibilities c]
                   (if (= c \X)
                     (concat (map #(conj % \0) possibilities)
                             (map #(conj % \1) possibilities))
                     (map #(conj % c) possibilities)))
                 '([]))
         (map (comp #(Long/parseLong % 2) (partial apply str))))))

(defn answer-part-2 [parsed-input]
  (->>
   parsed-input
   (reduce (fn [[mem mask] {:as command :keys [op value]}]
             (case op
               :set-mask [mem value]
               :set-mem  [(merge mem
                                 (zipmap (apply-mask2 (:address command) mask)
                                         (repeat value)))
                          mask]))
           [{} "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"])
   first
   vals
   (apply +)))

(def part-2-answer (answer-part-2 parsed-input))

(comment
  part-2-answer
  ;; => 3260587250457
  )
