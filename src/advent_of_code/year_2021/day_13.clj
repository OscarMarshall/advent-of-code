(ns advent-of-code.year-2021.day-13
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]
            [clojure.set :as set]))

(def input (core/get-input))

(defn parse-input [input]
  (let [[dots _ instructions] (partition-by #{""} (string/split-lines input))]
    {:dots         (into #{}
                         (map (comp (partial mapv #(Long/parseLong %))
                                    rest
                                    (partial re-matches #"(\d+),(\d+)")))
                         dots)
     :instructions (map (fn [line]
                          (let [[_ axis position]
                                (re-matches #"fold along ([xy])=(\d+)" line)]
                            {:axis     (keyword axis)
                             :position (Long/parseLong position)}))
                        instructions)}))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

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

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 775))


;;; Part 2
;;; ============================================================================

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

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer
           (str "███  ████ █  █ ███  █  █ ███  █  █ ███ \n"
                "█  █ █    █  █ █  █ █  █ █  █ █ █  █  █\n"
                "█  █ ███  █  █ █  █ █  █ █  █ ██   █  █\n"
                "███  █    █  █ ███  █  █ ███  █ █  ███ \n"
                "█ █  █    █  █ █    █  █ █    █ █  █ █ \n"
                "█  █ ████  ██  █     ██  █    █  █ █  █\n")))
