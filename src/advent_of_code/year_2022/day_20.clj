(ns advent-of-code.year-2022.day-20
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2022 20)


;;;; Parse

(defn parse-input [input]
  (let [lines      (string/split-lines input)
        line-count (count lines)]
    (into []
          (map-indexed (fn [index s]
                         {:id          index
                          :value       (parse-long s)
                          :previous-id (mod (dec index) line-count)
                          :next-id     (mod (inc index) line-count)}))
          lines)))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn remove-node [file {:keys [previous-id next-id]}]
  (-> file
      (assoc-in [previous-id :next-id] next-id)
      (assoc-in [next-id :previous-id] previous-id)))

(defn insert-after-node [file {:keys [next-id], previous-id :id} {node-id :id}]
  (-> file
      (update node-id assoc :previous-id previous-id, :next-id next-id)
      (assoc-in [previous-id :next-id] node-id)
      (assoc-in [next-id :previous-id] node-id)))

(defn find-first-index [col pred]
  (first (keep-indexed (fn [index x] (when (pred x) index)) col)))



(defn node-seq
  ([file] (node-seq file (find-first-index file (comp zero? :value))))
  ([file start-index] (iterate (comp file :next-id) (file start-index))))

(defn move-node [file node destination]
  (cond-> file
    (not= node destination) (-> (remove-node node)
                                (insert-after-node destination node))))

(defn move-number [file index]
  (let [{:as current-node, :keys [value]} (file index)]
    (move-node file
               current-node
               (nth (node-seq file index) (mod value (dec (count file)))))))

(defn mix-file [file]
  (reduce move-number file (range (count file))))

(defn answer [file times]
  (transduce (comp (take-nth 1000) (take 4) (map :value)) ; 0, 1000, 2000, 3000
             +
             (node-seq (nth (iterate mix-file file) times))))

(defn answer-part-1 [file] (answer file 1))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 2827])


;;;; Part 2

(defn decrypt [file key] (mapv #(update % :value * key) file))

(defn answer-part-2 [file] (answer (decrypt file 811589153) 10))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 7834270093909])
