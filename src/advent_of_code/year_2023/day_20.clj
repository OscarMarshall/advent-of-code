(ns advent-of-code.year-2023.day-20
  (:require [advent-of-code.core :as core]
            [clojure.string :as str]
            [medley.core :as medley]))

(println "# Day 20")

(set! *warn-on-reflection* true)

(defn parse-module [s]
  (let [[_ type name destinations] (re-matches #"([%&]?)([a-z]+) -> (.*)" s)]
    {:name         (keyword name)
     :type         (case type "%" :flip-flop, "&" :conjunction, :broadcaster)
     :destinations (map keyword (str/split destinations #", "))}))

(defn parse-input [input]
  (medley/index-by :name (map parse-module (str/split-lines input))))

;;;; Part 1

(defn gather-sources [modules]
  (transduce (mapcat (fn [{:keys [name destinations]}]
                       (map vector
                            destinations
                            (repeat name))))
             (completing (fn [acc [k v]] (update acc k (fnil conj []) v)))
             {}
             (vals modules)))

(defmulti initialize-module (fn [{:keys [type]} _] type))
(defmethod initialize-module :default [module _] module)
(defmethod initialize-module :flip-flop [module _] (assoc module :on false))
(defmethod initialize-module :conjunction [{:as module, :keys [name]} sources]
  (assoc module :signals (zipmap (sources name) (repeat false))))

(defmulti pulse-module
  (fn [modules module-name _source _high-pulse]
    (get-in modules [module-name :type])))
(defmethod pulse-module :default [modules _ _ _]
  [modules []])
(defmethod pulse-module :broadcaster [modules module-name _ high-pulse]
  [modules (map vector
                (repeat module-name)
                (repeat high-pulse)
                (get-in modules [module-name :destinations]))])
(defmethod pulse-module :flip-flop [modules module-name _ high-pulse]
  (if high-pulse
    [modules []]
    (let [{:keys [on]} (modules module-name)
          next-on      (not on)]
      [(assoc-in modules [module-name :on] next-on)
       (map vector
            (repeat module-name)
            (repeat next-on)
            (get-in modules [module-name :destinations]))])))
(defmethod pulse-module :conjunction [modules module-name source high-pulse]
  (let [{:keys [signals]} (modules module-name)
        signals           (assoc signals source high-pulse)]
    [(assoc-in modules [module-name :signals] signals)
     (map vector
          (repeat module-name)
          (repeat (boolean (some false? (vals signals))))
          (get-in modules [module-name :destinations]))]))

(defn initialize-modules [modules]
  (let [sources (gather-sources modules)]
    (medley/map-vals #(initialize-module % sources) modules)))

(defn propagate-pulses [modules pulses]
  (loop [modules modules, pulses (medley/queue pulses), pulse-counts [0 0]]
    (if (empty? pulses)
      [modules pulse-counts]
      (let [[source high-pulse destination] (peek pulses)
            [modules new-pulses]            (pulse-module modules
                                                          destination
                                                          source
                                                          high-pulse)]
        (recur modules
               (into (pop pulses) new-pulses)
               (update pulse-counts (if high-pulse 1 0) inc))))))

(defn press-button [modules n]
  (loop [modules (initialize-modules modules), n n, pulse-counts [0 0], seen {}]
    (cond
      (zero? n)      pulse-counts
      (seen modules) (let [[prev-n prev-pulse-counts] (seen modules)
                           cycle-length               (- prev-n n)]
                       (recur modules
                              (rem n cycle-length)
                              (mapv +
                                    pulse-counts
                                    (mapv #(* (quot n cycle-length) %)
                                          (mapv -
                                                pulse-counts
                                                prev-pulse-counts)))
                              {}))
      :else          (let [seen
                           (assoc seen modules [n pulse-counts])

                           [modules new-pulse-counts]
                           (propagate-pulses modules
                                             [[:button false :broadcaster]])]
                       (recur modules
                              (dec n)
                              (mapv + pulse-counts new-pulse-counts)
                              seen)))))

(defn answer-part-1 [modules]
  (apply * (press-button modules 1000)))

(core/part 1
  parse-input answer-part-1 *file*
  [:sample1 32000000]
  [:sample2 11687500]
  [:input 703315117])


;;;; Part 2

(defn answer-part-2 [x]
  x)

(core/part 2
  parse-input answer-part-2 *file*
  [:sample1 #_?]
  [:input #_(core/current-answer 2)])
