(ns advent-of-code.year-2022.day-19
  (:require [advent-of-code.core :as core]
            [medley.core :as medley]))

(set! *warn-on-reflection* true)

(core/set-date! 2022 19)


;;;; Parse

(def row-re
  #"Blueprint \d+: Each ore robot costs (\d+) ore\. Each clay robot costs (\d+) ore\. Each obsidian robot costs (\d+) ore and (\d+) clay\. Each geode robot costs (\d+) ore and (\d+) obsidian\.")

(defn row->recipes [[ore-robot-ore clay-robot-ore obsidian-robot-ore
                     obsidian-robot-clay geode-robot-ore geode-robot-obsidian]]
  {:ore      {:ore ore-robot-ore}
   :clay     {:ore clay-robot-ore}
   :obsidian {:ore obsidian-robot-ore, :clay obsidian-robot-clay}
   :geode    {:ore geode-robot-ore, :obsidian geode-robot-obsidian}})

(defn parse-input [input]
  (sequence (map (comp row->recipes (partial map parse-long) rest))
            (re-seq row-re input)))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn should-build? [{:keys [recipes robots]} robot]
  (or (= robot :geode)
      (< (robots robot 0)
         (transduce (comp (map val) (keep robot)) max 0 recipes))))

(defn can-build? [{:keys [inventory recipes]} robot]
  (every? (fn [[ingredient amount]] (>= (inventory ingredient) amount))
          (recipes robot)))

(defn produce
  ([state] (produce state 1))
  ([{:as state, :keys [robots]} time]
   (-> state
       (update :inventory
               (partial merge-with +)
               (update-vals robots (partial * time)))
       (update :time-remaining - time))))

(defn can-build-next? [{:as state, :keys [time-remaining]} robot]
  (can-build? (produce state time-remaining) robot))

(defn build [{:as state, :keys [recipes]} robot]
  (when (and (should-build? state robot) (can-build-next? state robot))
    (-> (medley/find-first #(can-build? % robot) (iterate produce state))
        (update :inventory (partial merge-with -) (recipes robot))
        produce
        (update-in [:robots robot] (fnil inc 0)))))

(defn geode-upper-bound [{:keys [inventory robots time-remaining]}]
  (let [geode-robots (:geode robots 0)]
    (reduce +
           (:geode inventory)
           (range geode-robots (+ geode-robots time-remaining)))))

(defn explore-possibilities [{:as state, :keys [recipes]}]
  (seq (keep (comp (partial build state) key) recipes)))

(defn max-geodes
  ([state] (max-geodes state 0))
  ([{:as state, :keys [time-remaining]} current-max]
   (if (< (geode-upper-bound state) current-max)
     0
     (if-some [possibilities (explore-possibilities state)]
       (reduce (fn [current-max state]
                 (max current-max (max-geodes state current-max)))
               current-max
               possibilities)
       (get-in (produce state time-remaining) [:inventory :geode])))))

(defn start-state [recipes time]
  {:inventory      (update-vals recipes (constantly 0))
   :recipes        recipes
   :robots         {:ore 1}
   :time-remaining time})

(defn answer-part-1 [parsed-input]
  (transduce (comp (map (comp max-geodes #(start-state % 24)))
                   (map-indexed (fn [index max-geodes]
                                  (* (inc index) max-geodes))))
             +
             parsed-input))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 1834])


;;;; Part 2

(defn answer-part-2 [parsed-input]
  (transduce (map (fn [recipes] (max-geodes (start-state recipes 32))))
             *
             (take 3 parsed-input)))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 2240])
