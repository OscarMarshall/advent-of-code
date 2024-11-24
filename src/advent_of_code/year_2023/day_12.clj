(ns advent-of-code.year-2023.day-12
  (:require [advent-of-code.core :as core]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)

(core/set-date! 2023 12)


;;;; Parse

(defn parse-input [input]
  (map (fn [s] (let [[springs damaged-groups] (str/split s #" " 2)]
                 [springs (map parse-long (str/split damaged-groups #","))]))
       (str/split-lines input)))

(core/set-parse-fn! parse-input)

;;;; Part 1

(def possible-arrangements
  (memoize
   (fn [springs damaged-groups]
     (if (< (count springs)
            (+ (apply + damaged-groups) (dec (count damaged-groups))))
       0
       (case (first springs)
         nil (if (empty? damaged-groups) 1 0)
         \.  (recur (rest springs) damaged-groups)
         \#  (if-some [damaged-group (first damaged-groups)]
               (if (and (every? #{\# \?} (take damaged-group springs))
                        (#{\. \?} (nth springs damaged-group \.)))
                 (recur (drop (inc damaged-group) springs)
                        (rest damaged-groups))
                 0)
               0)
         \?  (let [springs-tail (rest springs)]
               (+ (possible-arrangements (cons \# springs-tail) damaged-groups)
                  (possible-arrangements springs-tail damaged-groups))))))))

(defn answer-part-1 [condition-records]
  (transduce (map #(apply possible-arrangements %))
             +
             condition-records))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 21]
  [:puzzle 7195])


;;;; Part 2

(defn unfold-condition-record [[springs damaged-groups]]
  [(str/join "?" (repeat 5 springs)) (flatten (repeat 5 damaged-groups))])

(defn answer-part-2 [condition-records]
  (answer-part-1 (map unfold-condition-record condition-records)))

(core/set-answer-fn! 2 answer-part-2
  [:sample1 525152]
  [:puzzle 33992866292225])
