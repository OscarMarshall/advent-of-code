(ns user
  (:require
   [advent-of-code.core :as core]
   [clojure.string :as string]
   [portal.api :as portal])
  (:import (java.io File)
           (java.time LocalDate ZoneId)))
(comment
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
      (string/replace
       "HEADER"
       (format "# Year %1$d, Day %2$d: https://adventofcode.com/%1$d/day/%2$d"
               year
               day))))

(defn init-day! [year day]
  (reset! current-day [year day])
  (reset! samples 0)
  (when (.mkdirs (File. (format "src/advent_of_code/year_%d/" year)))
    (printf "Welcome to Advent of Code %d!%n"))
  (spit (format "src/advent_of_code/year_%d/day_%02d_input.txt" year day) "")
  (let [filename (code-file-name year day)]
    (spit filename (code-file-string year day))

    filename))

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
    [(keyword (str "sample" sample-id))]))
)

(defonce portal (atom nil))

(defn open-report! []
  (swap! portal #(portal/open %
                              {:theme    :portal.colors/solarized-dark
                               :value    core/report})))

(comment
  (portal/close portal)
  @core/report
  )
