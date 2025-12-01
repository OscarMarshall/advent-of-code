(ns advent-of-code.year-2024.day-23
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]))

(set! *warn-on-reflection* true)

(core/set-date! 2024 23)


;;;; Parse

(defn parse-input [input]
  (reduce (fn [acc [_ a b]]
            (-> acc
                (update a (fnil conj #{a}) b)
                (update b (fnil conj #{b}) a)))
          {}
          (re-seq #"(.+)-(.+)" input)))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn fully-connected? [connections computers]
  (every? (partial contains?
                   (apply set/intersection (map connections computers)))
          computers))

(defn answer-part-1 [computer->connections]
  (let [suspect-computers (filter #(string/starts-with? % "t")
                                  (keys computer->connections))]
    (->>
     suspect-computers
     (into
      #{}
      (mapcat
       (fn [computer1]
         (let [connections (computer->connections computer1)]
           (eduction (keep (fn [[computer2 computer3]]
                             (let [computers [computer1
                                              computer2
                                              computer3]]
                               (when (fully-connected? computer->connections
                                                       computers)
                                 (set computers)))))
                     (combo/combinations (disj connections
                                               computer1)
                                         2))))))
     count)))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 7]
  [:puzzle 1308])


;;;; Part 2

(defn bron-kerbosch [n r p x]
  (if (empty? p)
    (when (empty? x) (list r))
    (mapcat (fn [v p x]
              (let [neighbors (n v)]
                (bron-kerbosch n
                               (conj r v)
                               (set/intersection p neighbors)
                               (set/intersection x neighbors))))
            p
            (reductions disj p p)
            (reductions conj x p))))

(defn answer-part-2 [computer->connections]
  (->> #{}
       (bron-kerbosch (fn [computer]
                        (disj (computer->connections computer) computer))
                      #{}
                      (set (keys computer->connections)))
       (apply max-key count)
       sort
       (string/join ",")))

(core/set-answer-fn! 2 answer-part-2
    [:sample1 "co,de,ka,ta"]
    [:puzzle "bu,fq,fz,pn,rr,st,sv,tr,un,uy,zf,zi,zy"])
