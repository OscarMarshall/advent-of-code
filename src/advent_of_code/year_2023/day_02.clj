(ns advent-of-code.year-2023.day-02
  (:require [advent-of-code.core :as core]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)

(core/set-date! 2023 2)


;;;; Parse

(defn parse-subset [subset]
  (into {}
        (map (fn [[_ amount color]] [(keyword color) (parse-long amount)]))
        (re-seq #"(\d+) ([^,]+)" subset)))

(defn parse-game [game]
  (let [[_ id subsets] (re-matches #"Game (\d+): (.*)" game)]
    {:id (parse-long id)
     :subsets (map parse-subset (str/split subsets #"; "))}))

(defn parse-input [input] (map parse-game (str/split-lines input)))

(core/set-parse-fn! parse-input)


;;;; Part 1

(def full-set {:red 12, :green 13, :blue 14})

(defn possible-subset? [subset]
  (every? (fn [[color amount]] (<= amount (full-set color))) subset))

(defn possible-game? [{:keys [subsets]}] (every? possible-subset? subsets))

(defn answer-part-1 [games]
  (transduce (comp (filter possible-game?) (map :id)) + games))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 8]
  [:puzzle 2727])


;;;; Part 2

(defn smallest-full-set [{:keys [subsets]}] (apply merge-with max subsets))

(defn set-power [full-set] (apply * (vals full-set)))

(defn answer-part-2 [games]
  (transduce (map #(set-power (smallest-full-set %))) + games))

(core/set-answer-fn! 2 answer-part-2
  [:sample1 2286]
  [:puzzle 56580])
