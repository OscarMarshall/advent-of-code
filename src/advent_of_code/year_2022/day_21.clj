(ns advent-of-code.year-2022.day-21
  (:require [advent-of-code.core :as core]
            [clojure.core.logic :as logic]
            [clojure.core.logic.fd :as fd]
            [medley.core :as medley]))

(set! *warn-on-reflection* true)

(core/set-date! 2022 21)


;;;; Parse

(defn number-monkey [s] {:type :number, :value (parse-long s)})

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

(core/set-parse-fn! parse-input)


;;;; Part 1

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

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 21120928600114])


;;;; Part 2

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

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 3453748220116])
