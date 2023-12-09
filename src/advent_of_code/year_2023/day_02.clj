(ns advent-of-code.year-2023.day-02
  (:require [advent-of-code.core :as core]
            [clojure.string :as str]))

(println "# Day 2")

(set! *warn-on-reflection* true)

(defn parse-subset [subset]
  (into {}
        (map (fn [[_ amount color]] [(keyword color) (parse-long amount)]))
        (re-seq #"(\d+) ([^,]+)" subset)))

(defn parse-game [game]
  (let [[_ id subsets] (re-matches #"Game (\d+): (.*)" game)]
    {:id (parse-long id)
     :subsets (map parse-subset (str/split subsets #"; "))}))

(defn parse-input [input] (map parse-game (str/split-lines input)))


;;;; Part 1

(def full-set {:red 12, :green 13, :blue 14})

(defn possible-subset? [subset]
  (every? (fn [[color amount]] (<= amount (full-set color))) subset))

(defn possible-game? [{:keys [subsets]}] (every? possible-subset? subsets))

(defn answer-part-1 [games]
  (transduce (comp (filter possible-game?) (map :id)) + games))

(core/part 1
  parse-input answer-part-1 *file*
  [:sample1 8]
  [:input 2727])


;;;; Part 2

(defn smallest-full-set [{:keys [subsets]}] (apply merge-with max subsets))

(defn set-power [full-set] (apply * (vals full-set)))

(defn answer-part-2 [games]
  (transduce (map #(set-power (smallest-full-set %))) + games))

(core/part 2
  parse-input answer-part-2 *file*
  [:sample1 2286]
  [:input 56580])
