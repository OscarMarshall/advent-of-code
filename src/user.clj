(ns user
  (:require [clojure.string :as string])
  (:import (java.io File)))

(set! *warn-on-reflection* true)

(defn init-day [year day]
  (when (.mkdirs (File. (format "src/advent_of_code/year_%d/" year)))
    (printf "Welcome to Advent of Code %d!%n"))
  (spit (format "src/advent_of_code/year_%d/day_%02d.clj" year day)
        (-> "src/advent_of_code/day_00.clj"
            slurp
            (string/replace "advent-of-code.day-00"
                            (format "advent-of-code.year-%d.day-%02d" year day))
            (string/replace "00" (str day))))
  (spit (format "src/advent_of_code/year_%d/day_%02d_input.txt" year day) "")
  (spit (format "src/advent_of_code/year_%d/day_%02d_input_sample.txt" year day)
        ""))
