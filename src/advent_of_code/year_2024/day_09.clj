(ns advent-of-code.year-2024.day-09
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]
            [medley.core :as medley]))

(set! *warn-on-reflection* true)

(core/set-date! 2024 9)


;;;; Parse

(defn parse-input [input]
  (map (comp parse-long str) (string/trim input)))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn dense-format->blocks [dense-format]
  (map-indexed vector (mapcat repeat dense-format (interpose nil (range)))))

(defn compact-blocks [dense-format]
  (let [blocks (dense-format->blocks dense-format)]
    (->>
     blocks
     (reduce (fn [[result file-blocks-vector] [block-id file-id]]
               (let [[last-block-id last-file-id] (first file-blocks-vector)]
                 (cond
                   (= block-id last-block-id) (reduced (conj result file-id))
                   file-id                    [(conj result file-id)
                                               file-blocks-vector]
                   :else                      [(conj result last-file-id)
                                               (rest file-blocks-vector)])))
             [[] (filter (comp some? second) (reverse blocks))])
     (map-indexed vector))))

(defn blocks->checksum [blocks]
  (transduce (comp (filter second) (map (partial apply *))) + blocks))

(defn answer-part-1 [blocks]
  (blocks->checksum (compact-blocks blocks)))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 1928]
  [:puzzle 6398252054886])


;;;; Part 2

(defn dense-format->id-lengths [dense-format]
  (map vector (interpose nil (range)) dense-format))

(defn greedy-compact-blocks [id-lengths reverse-files]
  (loop [id-lengths    id-lengths
         reverse-files reverse-files
         seen          #{}
         result        []]
    (if-some [[file-id length :as block-span] (first id-lengths)]
      (let [id-lengths (rest id-lengths)]
        (if file-id
          (recur id-lengths
                 reverse-files
                 (conj seen file-id)
                 (conj result (cond-> block-span (seen file-id) (assoc 0 nil))))
          (let [reverse-files (drop-while (comp seen first) reverse-files)]
            (if-some [[moved-id moved-length :as moved-file]
                      (->> reverse-files
                           (remove (comp seen first))
                           (medley/find-first (fn [[_ last-length]]
                                                (<= last-length length))))]
              (let [new-length (- length moved-length)]
                (recur (cond-> id-lengths
                         (pos? new-length) (conj [nil new-length]))
                       reverse-files
                       (conj seen moved-id)
                       (conj result moved-file)))
              (recur id-lengths
                     reverse-files
                     seen
                     (conj result block-span))))))
      result)))

(defn id-lengths->blocks [dense-format-with-ids]
  (map-indexed vector (mapcat (fn [[id length]] (repeat length id))
                              dense-format-with-ids)))

(defn answer-part-2 [dense-format]
  (let [id-lengths (dense-format->id-lengths dense-format)]
    (-> id-lengths
        (greedy-compact-blocks (reverse (filter first id-lengths)))
        id-lengths->blocks
        blocks->checksum)))

(core/set-answer-fn! 2 answer-part-2
    [:sample1 2858]
    [:puzzle 6415666220005])
