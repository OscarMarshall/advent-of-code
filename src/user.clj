(ns user
  (:require
   [advent-of-code.core :as core]
   [clojure.string :as string]
   [portal.api :as portal])
  (:import (java.io File)
           (java.time LocalDate ZoneId)))

(set! *warn-on-reflection* true)


(defn code-file-name [year day]
  (format "src/advent_of_code/year_%d/day_%02d.clj" year day))

(defn code-file-string [year day]
  (-> "src/advent_of_code/day_00.clj"
      slurp
      (string/replace "advent-of-code.day-00"
                      (format "advent-of-code.year-%d.day-%02d" year day))
      (string/replace "'year" (str year))
      (string/replace "'day" (str day))))

(defn init-day! [year day]
  (when (.mkdirs (File. (format "src/advent_of_code/year_%d/" year)))
    (printf "Welcome to Advent of Code %d!%n"))
  (spit (code-file-name year day) (code-file-string year day)))

(defn init-today! []
  (let [date (LocalDate/now (ZoneId/of "America/New_York"))]
    (init-day! (.getYear date) (.getDayOfMonth date))))

(defn init-tomorrow! []
  (let [date (LocalDate/now (ZoneId/of "America/New_York"))]
    (init-day! (.getYear date) (inc (.getDayOfMonth date)))))

(defn add-sample! [s]
  (let [{:keys [year day]} @core/state
        sample-id          (->> (range)
                                rest
                                (map (fn [n] (keyword (str "sample" n))))
                                (remove (set (keys (core/get-inputs year day))))
                                first)]
    (spit (format "src/advent_of_code/year_%d/day_%02d_input_%s.txt"
                  year
                  day
                  (name sample-id))
          s)
    [sample-id]))

(defonce portal (atom nil))

(defn open-report! []
  (swap! portal #(portal/open %
                              {:theme :portal.colors/solarized-dark
                               :value core/report})))

(defn portal-value [] @@portal)
