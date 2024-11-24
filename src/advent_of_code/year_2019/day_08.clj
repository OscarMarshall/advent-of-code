(ns advent-of-code.year-2019.day-08
  (:require [advent-of-code.core :as core]
            [clojure.edn :as edn]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2019 8)


;;;; Parse

(def width 25)
(def height 6)

(defn parse-input [input]
  (partition (* width height) (map (comp parse-long str) input)))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn answer-part-1 [layers]
  (let [{ones 1, twos 2} (apply min-key #(get % 0 0) (map frequencies layers))]
    (* ones twos)))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 828])


;;;; Part 2

(defn answer-part-2 [layers]
  (->> layers
       (apply map (fn [& pixels]
                    (case (int (first (drop-while #{2} pixels)))
                      0 " "
                      1 "█")))
       (partition width)
       (map (partial apply str))
       (string/join "\n")))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle (str "████ █    ███    ██ ████ \n"
                "   █ █    █  █    █ █    \n"
                "  █  █    ███     █ ███  \n"
                " █   █    █  █    █ █    \n"
                "█    █    █  █ █  █ █    \n"
                "████ ████ ███   ██  █    ")])
