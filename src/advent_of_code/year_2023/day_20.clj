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
  (loop [modules modules, pulses pulses, i 0]
    (if-some [[source high-pulse destination] (nth pulses i nil)]
      (let [[modules new-pulses] (pulse-module modules
                                               destination
                                               source
                                               high-pulse)]
        (recur modules (into pulses new-pulses) (inc i)))
      [modules pulses])))

(defn press-button [modules]
  (propagate-pulses modules [[:button false :broadcaster]]))

(defn button-presses [modules]
  (iterate (fn [[modules _]] (press-button modules))
           [(initialize-modules modules)]))

(defn answer-part-1 [modules]
  (transduce (comp (take 1000) (mapcat second))
             (fn
               ([[low high]] (* low high))
               ([acc [_ high _]] (update acc (if high 1 0) inc)))
             [0 0]
             (rest (button-presses modules))))

(core/part 1
  parse-input answer-part-1 *file*
  [:sample1 32000000]
  [:sample2 11687500]
  [:input 703315117])


;;;; Part 2

(defmulti current-signal (fn [{:keys [type]}] type))
(defmethod current-signal :flip-flop [{:keys [on]}] on)
(defmethod current-signal :conjunction [{:keys [signals]}]
  (boolean (some false? (vals signals))))

(defn cycle-length [modules source high]
  (->> (button-presses modules)
       (take-while (fn [[_ pulses]]
                     (not-any? (fn [[pulse-source pulse-high _]]
                                 (and (= pulse-source source)
                                      (= pulse-high high)))
                               pulses)))
       count))

(defn answer-part-2 [modules]
  (let [sources (gather-sources modules)]
    (transduce (map #(cycle-length modules % true))
               *
               (sources (first (sources :rx))))))

(core/part 2
  parse-input answer-part-2 *file*
  [:input 230402300925361])
