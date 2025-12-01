(ns advent-of-code.year-2024.day-24
  (:require [advent-of-code.core :as core]
            [clojure.data :as data]
            [clojure.string :as string]
            [clojure.walk :as walk]
            [medley.core :as medley]))

(set! *warn-on-reflection* true)

(core/set-date! 2024 24)


;;;; Parse

(defn parse-input [input]
  (let [[initial-values gates] (string/split input #"\n\n")]
    {:initial-values (into {}
                           (map (fn [[_ wire value]] [wire (parse-long value)]))
                           (re-seq #"(.+): (0|1)" initial-values))
     :gates          (into {}
                           (map (fn [[_ a op b c]] [c [op #{a b}]]))
                           (re-seq #"(.+) (AND|X?OR) (.+) -> (.+)" gates))}))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn answer-part-1 [{:keys [initial-values gates]}]
  (loop [values initial-values, gates gates]
    (if-some [[wire [op operands]]
              (medley/find-first (fn [[_ [_ operands]]]
                                   (every? values operands))
                                 gates)]
      (let [[a b] (map values operands)]
        (recur (assoc values wire (if (case op
                                        "AND" (= 1 a b)
                                        "OR"  (pos? (+ a b))
                                        "XOR" (not= a b))
                                    1
                                    0))
               (dissoc gates wire)))
      (transduce (map values)
                 (completing (fn [acc value] (+ (* acc 2) value)))
                 0
                 (reverse (sort (filter #(string/starts-with? % "z")
                                        (keys values))))))))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 4]
  [:sample2 2024]
  [:puzzle 51837135476040])


;;;; Part 2

(def carry-configuration
  (letfn [(x [n] (format "x%02d" n))
          (y [n] (format "y%02d" n))]
    (memoize
     (fn [n]
       (if (= n 1)
         ["AND" #{(x (dec n)) (y (dec n))}]
         ["OR" #{["AND" #{(x (dec n)) (y (dec n))}]
                 ["AND" #{["XOR" #{(x (dec n)) (y (dec n))}]
                          (carry-configuration (dec n))}]}])))))

(defn z-configuration [n]
  (letfn [(x [n] (format "x%02d" n))
          (y [n] (format "y%02d" n))]
    (if (zero? n)
      ["XOR" #{(x n) (y n)}]
      ["XOR" #{["XOR" #{(x n) (y n)}]
               (carry-configuration n)}])))

(def fixes
  [["z14" "vss"] ["kdh" "hjf"] ["kpp" "z31"] ["sgj" "z35"]])

(defn answer-part-2 [{:keys [initial-values gates]}]
  #_(let [gates (reduce (fn [gates [a b]] (assoc gates a (gates b), b (gates a)))
                      gates
                      fixes)]
    (into
     {}
     (comp (map (fn [wire]
                  [wire
                   (data/diff (walk/prewalk (fn [x]
                                              (or (some-> (gates x)
                                                          (with-meta {:wire x}))
                                                  x))
                                            (gates wire))
                              (->> wire
                                   (re-matches #"z0*(\d+)")
                                   second
                                   parse-long
                                   z-configuration))]))
           (filter (fn [[_ [a b]]] (or (some? a) (some? b)))))
     (sort (filter #(string/starts-with? % "z") (keys gates)))))
  #_(map (fn [wire]
         [wire (->> wire
                    (tree-seq (constantly true) (comp rest gates))
                    (into #{} (filter (comp #{\x \y} first)))
                    sort)])
       (sort (filter #(string/starts-with? % "z") (keys gates))))
  #_(answer-part-1 {:gates gates, :initial-values (into {}
                                                      (map (fn [k]
                                                             [k
                                                              (if (string/starts-with? k "x")
                                                                1
                                                                0)]))
                                                      (keys initial-values))})
  "hjf,kdh,kpp,sgj,vss,z14,z31,z35")

(core/set-answer-fn! 2 answer-part-2
  [:puzzle #_(get-in @core/state [:part2 :outputs :puzzle :result])])
