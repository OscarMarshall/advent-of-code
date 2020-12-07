(ns user
  (:require [clojure.string :as string]))

(defn init-day [day]
  (-> "src/advent_of_code_2020/day_00.clj"
      slurp
      (string/replace "00" (format "%02d" day))
      (->> (spit (format "src/advent_of_code_2020/day_%02d.clj" day))))
  (spit (format "src/advent_of_code_2020/day_%02d_input.txt" day) ""))
