(ns advent-of-code.year-2022.day-22
  (:require [advent-of-code.core :as core]
            [clojure.edn :as edn]
            [clojure.string :as string]
            [medley.core :as medley]))

(set! *warn-on-reflection* true)

(def input (core/get-input *file*))

(defn parse-input [input]
  (let [[board path] (string/split input #"\n\n")]
    {:board (string/split-lines board)
     :path (map (fn [s] (or ({"R" :right, "L" :left} s) (edn/read-string s)))
                (re-seq #"\d+|[RL]" path))}))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

(def direction->delta {0 [0 1], 1 [1 0], 2 [0 -1], 3 [-1 0]})

(defn step [[row column] direction]
  (let [[row-delta column-delta] (direction->delta direction)]
    [[(+ row row-delta) (+ column column-delta)] direction]))

(defn wrap-step [board position direction]
  [(mapv mod
         (first (step position direction))
         [(count board) (count (first board))])
   direction])

(defn repeat-last [col] (concat col (lazy-seq (repeat (last col)))))

(defn walk [step-fn board position direction steps]
  (-> [position direction]
      (->> (iterate (partial apply step-fn board))
           (remove (comp (some-fn nil? #{\space}) (partial get-in board) first))
           cycle
           (take-while (comp #{\.} (partial get-in board) first))
           repeat-last)
      (nth steps)))

(defmulti execute-instruction (fn [_ direction _] direction))
(defmethod execute-instruction :left [[position direction] _ _]
  [position (mod (dec direction) 4)])
(defmethod execute-instruction :right [[position direction] _ _]
  [position (mod (inc direction) 4)])
(defmethod execute-instruction :default
  [[position direction] steps {:keys [board step-fn]}]
  (walk step-fn board position direction steps))

(defn answer [step-fn {:keys [board path]}]
  (let [top-left [0 (.indexOf ^String (first board) ".")]
        [[row column] direction]
        (reduce #(execute-instruction %1 %2 {:board board, :step-fn step-fn})
                [top-left 0]
                path)]
    (+ (* (inc row) 1000) (* (inc column) 4) direction)))

(def answer-part-1 (partial answer wrap-step))

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 181128))


;;; Part 2
;;; ============================================================================

(def cube-face-starts [[0 50] [0 100] [50 50] [100 0] [100 50] [150 0]])

(def transitions
  [[[1 0] [2 1] [3 0] [5 0]]
   [[4 2] [2 2] [0 2] [5 3]]
   [[1 3] [4 1] [3 1] [0 3]]
   [[4 0] [5 1] [0 0] [2 0]]
   [[1 2] [5 2] [3 2] [2 3]]
   [[4 3] [1 1] [0 1] [3 3]]])

(defn cube-face [position]
  (some-> (medley/find-first (comp (partial every? identity)
                                   (fn [starts]
                                     (map (fn [start coordinate]
                                            (<= start coordinate (+ start 49)))
                                          starts
                                          position))
                                   second)
                             (medley/indexed cube-face-starts))
          first))

(defn global-position->local-position [global-position face]
  (mapv - global-position (cube-face-starts face)))

(defn local-position->global-position [local-position face]
  (mapv + local-position (cube-face-starts face)))

(defn transitioning? [[row column] ^long direction]
  (case direction 0 (= column 49), 1 (= row 49), 2 (= column 0), 3 (= row 0)))

(defn edge-offset [[row column] ^long direction]
  (case direction 0 row, 1 (- 49 column), 2 (- 49 row), 3 column))

(defn transition-face [local-position face ^long direction]
  (let [offset             (edge-offset local-position direction)
        [face* direction*] (get-in transitions [face direction])]
    [(case (long direction*)
       0 [offset 0]
       1 [0 (- 49 offset)]
       2 [(- 49 offset) 49]
       3 [49 offset])
     face*
     direction*]))

(defn cube-step [_ global-position direction]
  (let [face (cube-face global-position)
        local-position (global-position->local-position global-position face)]
    (if (transitioning? local-position direction)
      (let [[local-position face direction] (transition-face local-position
                                                             face
                                                             direction)]
        [(local-position->global-position local-position face) direction])
      (step global-position direction))))

(defn answer-part-2 [state] (answer cube-step state))

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer 52311))
