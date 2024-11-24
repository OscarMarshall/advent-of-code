(ns advent-of-code.year-2019.day-11
  (:require [advent-of-code.core :as core]
            [advent-of-code.year-2019.intcode-computer :as intcode-computer]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2019 11)


;;;; Parse

(core/set-parse-fn! intcode-computer/parse-program)


;;;; Part 1

(def movement-vectors
  {:up    [0 1]
   :right [1 0]
   :down  [0 -1]
   :left  [-1 0]})

(def turn
  {:up    [:left :right]
   :right [:up :down]
   :down  [:right :left]
   :left  [:down :up]})

(defn paint [hull state position direction]
  (if (intcode-computer/halted? state)
    hull
    (let [state              (intcode-computer/continue state (hull position 0))
          [color state]      (intcode-computer/read-output state)
          [turn-index state] (intcode-computer/read-output state)
          direction          (nth (turn direction) turn-index)]
      (recur (assoc hull position color)
             state
             (mapv + position (movement-vectors direction))
             direction))))

(defn answer-part-1 [program]
  (count (paint {} (intcode-computer/start program) [0 0] :up)))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 2322])


;;;; Part 2

(defn hull-string [hull]
  (let [positions (keys hull)
        xs        (map first positions)
        min-x     (apply min xs)
        max-x     (apply max xs)
        ys        (map second positions)
        min-y     (apply min ys)
        max-y     (apply max ys)]
    (string/join "\n" (map (fn [y]
                             (string/join (map (fn [x]
                                                 (if (= (hull [x y]) 1)
                                                   "█"
                                                   " "))
                                               (range min-x (inc max-x)))))
                           (range max-y (dec min-y) -1)))))

(defn answer-part-2 [program]
  (hull-string (paint {[0 0] 1} (intcode-computer/start program) [0 0] :up)))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle (str "   ██ █  █  ██  ███  ███   ██   ██  █  █   \n"
                "    █ █  █ █  █ █  █ █  █ █  █ █  █ █  █   \n"
                "    █ ████ █  █ █  █ ███  █    █    █  █   \n"
                "    █ █  █ ████ ███  █  █ █ ██ █    █  █   \n"
                " █  █ █  █ █  █ █ █  █  █ █  █ █  █ █  █   \n"
                "  ██  █  █ █  █ █  █ ███   ███  ██   ██    ")])
