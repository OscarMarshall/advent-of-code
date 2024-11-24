(ns advent-of-code.year-2021.day-16
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2021 16)


;;;; Parse

(def hex->binary-sequence
  {\0 '(0 0 0 0)
   \1 '(0 0 0 1)
   \2 '(0 0 1 0)
   \3 '(0 0 1 1)
   \4 '(0 1 0 0)
   \5 '(0 1 0 1)
   \6 '(0 1 1 0)
   \7 '(0 1 1 1)
   \8 '(1 0 0 0)
   \9 '(1 0 0 1)
   \A '(1 0 1 0)
   \B '(1 0 1 1)
   \C '(1 1 0 0)
   \D '(1 1 0 1)
   \E '(1 1 1 0)
   \F '(1 1 1 1)})

(defn parse-input [input] (mapcat hex->binary-sequence (string/trim input)))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn binary-sequence->long [binary-sequence]
  (reduce (fn [acc x] (+ (bit-shift-left acc 1) x)) 0 binary-sequence))

(defn read-bits [bits n]
  (let [[result bits] (split-at n bits)]
    [(binary-sequence->long result) bits]))

(defn read-integer [bits]
  (let [half-bytes-count (inc (count (take-while #{[1]} (partition 1 5 bits))))
        binary-sequence  (mapcat rest
                                 (take half-bytes-count (partition 5 bits)))]
    [(binary-sequence->long binary-sequence)
     (drop (* half-bytes-count 5) bits)]))

(defn parse-packet [bits]
  (let [[version bits] (read-bits bits 3)
        [type bits]    (read-bits bits 3)
        packet         {:version version, :type type}]
    (if (= type 4)
      (let [[integer bits] (read-integer bits)]
        [(assoc packet :body integer) bits])
      (let [[length-type-id bits] (read-bits bits 1)]
        (if (zero? length-type-id)
          (let [[sub-packet-bits-count bits] (read-bits bits 15)
                [sub-packet-bits bits]       (split-at sub-packet-bits-count
                                                       bits)
                packets                      (loop [bits   sub-packet-bits
                                                    result []]
                                               (if (empty? bits)
                                                 result
                                                 (let [[packet bits]
                                                       (parse-packet bits)]
                                                   (recur bits
                                                          (conj result
                                                                packet)))))]
            [(assoc packet :body packets) bits])
          (let [[sub-packets-count bits] (read-bits bits 11)
                [packets bits]           (reduce (fn [[packets bits] _]
                                                   (let [[packet bits]
                                                         (parse-packet bits)]
                                                     [(conj packets packet)
                                                      bits]))
                                                 [[] bits]
                                                 (range sub-packets-count))]
            [(assoc packet :body packets) bits]))))))

(defn sum-of-version-numbers [{:keys [version type body]}]
  (apply + version (when (not= type 4) (map sum-of-version-numbers body))))

(defn answer-part-1 [parsed-input]
  (sum-of-version-numbers (first (parse-packet parsed-input))))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 893])


;;;; Part 2

(defn comparison [f]
  (fn [x y] (if (f x y) 1 0)))

(defn evaluate-packet [{:keys [type body]}]
  (if (= type 4)
    body
    (apply (case (int type)
             0 +
             1 *
             2 min
             3 max
             5 (comparison >)
             6 (comparison <)
             7 (comparison =))
           (map evaluate-packet body))))

(defn answer-part-2 [parsed-input]
  (evaluate-packet (first (parse-packet parsed-input))))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 4358595186090])
