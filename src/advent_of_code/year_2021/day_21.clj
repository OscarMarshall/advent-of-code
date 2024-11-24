(ns advent-of-code.year-2021.day-21
  (:require [advent-of-code.core :as core]))

(set! *warn-on-reflection* true)

(core/set-date! 2021 21)


;;;; Parse

(defn parse-input [input]
  (mapv (comp parse-long second)
        (re-seq #"Player [12] starting position: (\d+)" input)))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn play-turn [[turn posns scores] die-rolls]
  (let [player (mod turn 2)
        posns  (update posns
                       player
                       (fn [posn]
                         (inc (mod (apply + (dec posn) die-rolls) 10))))]
    [(inc turn) posns (update scores player + (posns player))]))

(defn winner-fn [target]
  (fn [[_ _ [player-1-score player-2-score]]]
    (condp >= target
      player-1-score 0
      player-2-score 1
      nil)))

(defn answer-part-1 [parsed-input]
  (let [[turn _ scores] (->> (range)
                             rest
                             (take 100)
                             cycle
                             (partition 3)
                             (reductions play-turn [0 parsed-input [0 0]])
                             (drop-while (complement (winner-fn 1000)))
                             first)]
    (* (apply min scores) 3 turn)))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 1002474])


;;;; Part 2

(def dirac-wins
  (memoize
   (fn [state winner]
     (if-some [winner (winner state)]
       (update [0 0] winner inc)
       (reduce
        (partial map +)
        [;; (1 1 1)
         (dirac-wins (play-turn state '(3)) winner)
         ;; (1 1 2) (1 2 1) (2 1 1)
         (map (partial * 3) (dirac-wins (play-turn state '(4)) winner))
         ;; (1 1 3) (1 2 2) (1 3 1) (2 1 2) (2 2 1) (3 1 1)
         (map (partial * 6) (dirac-wins (play-turn state '(5)) winner))
         ;; (1 2 3) (1 3 2) (2 1 3) (2 2 2) (2 3 1) (3 1 2) (3 2 1)
         (map (partial * 7) (dirac-wins (play-turn state '(6)) winner))
         ;; (1 3 3) (2 2 3) (2 3 2) (3 1 3) (3 2 2) (3 3 1)
         (map (partial * 6) (dirac-wins (play-turn state '(7)) winner))
         ;; (2 3 3) (3 2 3) (3 3 2)
         (map (partial * 3) (dirac-wins (play-turn state '(8)) winner))
         ;; (3 3 3)
         (map (partial * 1) (dirac-wins (play-turn state '(9)) winner))])))))

(defn answer-part-2 [parsed-input]
  (apply max (dirac-wins [0 parsed-input [0 0]] (winner-fn 21))))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 919758187195363])
