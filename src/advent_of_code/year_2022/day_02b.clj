(ns advent-of-code.year-2022.day-02b
  (:require [advent-of-code.core :as core]
            [clojure.core.logic :as logic]
            [clojure.core.logic.pldb :as pldb]
            [clojure.string :as string]))

(def input (core/get-input))

(defn parse-input [input]
  (mapv (fn [s] (mapv keyword (string/split s #" ")))
        (string/split-lines input)))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

(pldb/db-rel rps me opponent outcome)

(def rps-choices [:rock :paper :scissors])
(def abc->rps (zipmap [:A :B :C] rps-choices))
(def xyz->rps (zipmap [:X :Y :Z] rps-choices))
(def rps->score (zipmap rps-choices (rest (range))))
(def outcome->score {:loss 0, :draw 3, :win 6})
(def facts
  (apply pldb/db (mapcat (fn [n outcome]
                           (map (fn [me opponent] [rps me opponent outcome])
                                rps-choices
                                (drop n (cycle rps-choices))))
                         (range 3)
                         [:draw :loss :win])))

(defn score [me outcome] (+ (rps->score me) (outcome->score outcome)))

(defn answer-part-1 [parsed-input]
  (transduce (map (fn [[abc xyz]]
                    (let [me       (xyz->rps xyz)
                          opponent (abc->rps abc)
                          outcome  (first (pldb/with-db facts
                                            (logic/run 1 [outcome]
                                              (rps me opponent outcome))))]
                      (score me outcome))))
             +
             parsed-input))

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 12156))


;;; Part 2
;;; ============================================================================

(def xyz->outcome {:X :loss, :Y :draw, :Z :win})

(defn answer-part-2 [parsed-input]
  (transduce (map (fn [[abc xyz]]
                    (let [opponent (abc->rps abc)
                          outcome  (xyz->outcome xyz)
                          me       (first (pldb/with-db facts
                                            (logic/run 1 [me]
                                              (rps me opponent outcome))))]
                      (score me outcome))))
             +
             parsed-input))

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer 10835))
