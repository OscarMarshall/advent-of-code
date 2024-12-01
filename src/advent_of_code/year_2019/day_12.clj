(ns advent-of-code.year-2019.day-12
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2019 12)


;;;; Parse

(defn parse-input [input]
  (map (fn [line]
         (mapv parse-long
               (subvec (re-matches #"<x=([^,]*), y=([^,]*), z=([^,]*)>" line)
                       1)))
       (string/split-lines input)))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn answer-part-1 [x]
  x)

(core/set-answer-fn! 1 answer-part-1
    [:puzzle #_(get-in core/state [:outputs :puzzle])]
    #_(user/add-sample! ""))


;;;; Part 2

(defn answer-part-2 [x]
  x)

#_(core/set-answer-fn! 2 answer-part-2
    [:puzzle #_(get-in core/state [:outputs :puzzle])]
    #_(user/add-sample! ""))
