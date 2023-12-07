(ns advent-of-code.year-2022.day-21
  (:require [advent-of-code.core :as core]
            [clojure.core.logic :as logic]
            [clojure.core.logic.fd :as fd]
            [clojure.edn :as edn]
            [medley.core :as medley]))

(def input (core/get-input *file*))

(defn number-monkey [s] {:type :number, :value (edn/read-string s)})

(defn operation-monkey [[_ left operation right]]
  {:type      :operation
   :operation (keyword operation)
   :operands  [(keyword left) (keyword right)]})

(defn make-monkey [s]
  (condp re-matches s
    #"\d+"                             :>> number-monkey
    #"([a-z]{4}) ([+\-*/]) ([a-z]{4})" :>> operation-monkey))

(defn parse-input [input]
  (->> input
       (re-seq #"([a-z]{4}): (.*)\n")
       (into {} (map (fn [[_ monkey-id value]]
                       [(keyword monkey-id) (make-monkey value)])))))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

(def operation->goal {:+ fd/+, :- fd/-, :* fd/*, :/ fd/quot})

(defn relate-subtree [monkeys monkey-id]
  (let [{:keys [lvar operands operation type value]} (monkeys monkey-id)]
    (logic/all
     (fd/in lvar (fd/interval (bigint Long/MAX_VALUE)))
     (case type
       :number    (fd/== lvar value)
       :operation (let [[left right] operands]
                    (logic/all
                     (relate-subtree monkeys left)
                     (relate-subtree monkeys right)
                     (let [left  (get-in monkeys [left :lvar])
                           right (get-in monkeys [right :lvar])]
                       (case operation
                         (:+ :- :* :/) ((operation->goal operation)
                                        left
                                        right
                                        lvar)
                         :=            (fd/== left right)))))
       :lvar      logic/succeed))))

(defn answer-part-1 [parsed-input]
  (let [monkeys (medley/map-kv-vals (fn [id monkey]
                                      (assoc monkey
                                             :lvar (logic/lvar (name id))))
                                    parsed-input)]
    (first (logic/run 1 [out]
             (relate-subtree monkeys :root)
             (logic/== out (get-in monkeys [:root :lvar]))))))

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 21120928600114))

(< 21120928600114 (quot Long/MAX_VALUE 4))


;;; Part 2
;;; ============================================================================

(defn answer-part-2 [parsed-input]
  (let [monkeys
        (-> parsed-input
            (assoc-in [:root :operation] :=)
            (assoc-in [:humn :type] :lvar)
            (->> (medley/map-kv-vals (fn [id monkey]
                                       (assoc monkey
                                              :lvar (logic/lvar (name id)))))))]
    (first (logic/run 1 [out]
             (relate-subtree monkeys :root)
             (logic/== out (get-in monkeys [:humn :lvar]))))))

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer 3453748220116))
