(ns user
  (:require
   #_:clj-kondo/ignore
   [advent-of-code.core :refer [current-parsed-input current-answer]]
   [clojure.string :as string])
  (:import (java.io File)
           (java.time LocalDate ZoneId)))

(set! *warn-on-reflection* true)

(def current-day (atom [0 0]))
(def samples (atom 0))

(defn code-file-name [year day]
  (format "src/advent_of_code/year_%d/day_%02d.clj" year day))

(defn code-file-string [year day]
  (-> "src/advent_of_code/day_00.clj"
      slurp
      (string/replace "advent-of-code.day-00"
                      (format "advent-of-code.year-%d.day-%02d" year day))
      (string/replace "00" (str day))))

(defn init-day! [year day]
  (let []
    (reset! current-day [year day])
    (reset! samples 0)
    (when (.mkdirs (File. (format "src/advent_of_code/year_%d/" year)))
      (printf "Welcome to Advent of Code %d!%n"))
    (spit (code-file-name year day) (code-file-string year day))
    (spit (format "src/advent_of_code/year_%d/day_%02d_input.txt" year day)
          "")))

(defn init-today! []
  (let [date (LocalDate/now (ZoneId/of "America/New_York"))]
    (init-day! (.getYear date) (.getDayOfMonth date))))

(defn init-tomorrow! []
  (let [date (LocalDate/now (ZoneId/of "America/New_York"))]
    (init-day! (.getYear date) (inc (.getDayOfMonth date)))))

(defn add-sample! [s]
  (let [[year day] @current-day
        sample-id  (swap! samples inc)]
    (spit (format "src/advent_of_code/year_%d/day_%02d_input_sample%d.txt"
                  year
                  day
                  sample-id)
          s)
    (keyword (str "sample" sample-id))))
