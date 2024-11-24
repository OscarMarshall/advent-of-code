(ns advent-of-code.year-2021.day-13
  (:require [advent-of-code.core :as core]
            [clojure.set :as set]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2021 13)


;;;; Parse

(defn parse-input [input]
  (let [[dots _ instructions] (partition-by #{""} (string/split-lines input))]
    {:dots         (into #{}
                         (map (comp (partial mapv parse-long)
                                    rest
                                    (partial re-matches #"(\d+),(\d+)")))
                         dots)
     :instructions (map (fn [line]
                          (let [[_ axis position]
                                (re-matches #"fold along ([xy])=(\d+)" line)]
                            {:axis     (keyword axis)
                             :position (parse-long position)}))
                        instructions)}))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn fold [dots {:keys [axis position]}]
  (let [axis         ({:x 0, :y 1} axis)
        dots-to-fold (into #{}
                           (filter (fn [dot] (> (nth dot axis) position)))
                           dots)]
    (-> dots
        (set/difference dots-to-fold)
        (into (map (fn [dot]
                     (update dot axis (fn [coord]
                                        (- position (- coord position))))))
              dots-to-fold))))

(defn answer-part-1 [{:keys [dots instructions]}]
  (count (fold dots (first instructions))))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 775])


;;;; Part 2

(defn dots-str [dots]
  (let [width  (apply max (map #(nth % 0) dots))
        height (apply max (map #(nth % 1) dots))]
    (apply str (mapcat (fn [y]
                         (concat (map (fn [x] (if (dots [x y]) \█ \space))
                                      (range (inc width)))
                                 [\newline]))
                       (range (inc height))))))

(defn answer-part-2 [{:keys [dots instructions]}]
  (dots-str (reduce fold dots instructions)))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle (str "███  ████ █  █ ███  █  █ ███  █  █ ███ \n"
                "█  █ █    █  █ █  █ █  █ █  █ █ █  █  █\n"
                "█  █ ███  █  █ █  █ █  █ █  █ ██   █  █\n"
                "███  █    █  █ ███  █  █ ███  █ █  ███ \n"
                "█ █  █    █  █ █    █  █ █    █ █  █ █ \n"
                "█  █ ████  ██  █     ██  █    █  █ █  █\n")])
