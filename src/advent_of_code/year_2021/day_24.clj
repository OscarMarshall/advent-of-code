(ns advent-of-code.year-2021.day-24
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2021 24)


;;;; Parse

(defn parse-input [input]
  (map (fn [s]
         (let [[_ op a b] (re-matches #"(...) (.)(?: (.+))?" s)]
           {:op (keyword op)
            :a  (keyword a)
            :b  (cond
                  (nil? b)                nil
                  (re-matches #"[w-z]" b) (keyword b)
                  :else                   (parse-long b))}))
       (string/split-lines input)))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defmulti execute-instruction (fn [_ {:keys [op]}] op))
(defmethod execute-instruction :inp [{:as state, :keys [input]} {:keys [a]}]
  (-> state
      (update :input rest)
      (assoc a (first input))))
(defmethod execute-instruction :add [state {:keys [a b]}]
  (update state a (partial mapv + (if (keyword? b) (state b) [b b]))))
(defmethod execute-instruction :mul [state {:keys [a b]}]
  (update state a (fn [[a-min a-max]]
                    (let [[b-min b-max] (if (keyword? b) (state b) [b b])
                          extremes      [(* a-min b-min)
                                         (* a-min b-max)
                                         (* a-max b-min)
                                         (* a-max b-max)]]
                      [(apply min extremes) (apply max extremes)]))))
(defmethod execute-instruction :div [state {:keys [a b]}]
  (update state a (fn [[a-min a-max]]
                    (let [[b-min b-max] (if (keyword? b) (state b) [b b])
                          extremes      [(quot a-min b-min)
                                         (quot a-min b-max)
                                         (quot a-max b-min)
                                         (quot a-max b-max)]]
                      [(apply min extremes) (apply max extremes)]))))
(defmethod execute-instruction :mod [state {:keys [a b]}]
  (update state
          a
          (fn [[a-min a-max]]
            (let [[b-min b-max] (if (keyword? b) (state b) [b b])]
              (cond
                (and (= a-min a-max) (= b-min b-max)) (let [x (rem a-min b-min)]
                                                        [x x])
                (>= a-max b-max)                      [0 (dec b-max)]
                :else                                 [a-min a-max])))))
(defmethod execute-instruction :eql [state {:keys [a b]}]
  (update state a (fn [[a-min a-max]]
                    (let [[b-min b-max] (if (keyword? b) (state b) [b b])]
                      (cond
                        (= a-min a-max b-min b-max)          [1 1]
                        (or (< a-max b-min) (< b-max a-min)) [0 0]
                        :else                                [0 1])))))

(defn run-program [instructions input]
  (:z (reduce execute-instruction
              {:input (concat (map (juxt identity identity) input)
                              (repeat [1 9]))
               :w     [0 0]
               :x     [0 0]
               :y     [0 0]
               :z     [0 0]}
              instructions)))

(defn find-serial [program largest prefix]
  (if (= (count prefix) 14)
    prefix
    (->> (cond-> (range 1 10) largest reverse)
         (filter (fn [x]
                   (let [[z-min z-max] (run-program program (conj prefix x))]
                     (<= z-min 0 z-max))))
         (some (comp (partial find-serial program largest)
                     (partial conj prefix))))))

(defn input->serial-number [xs]
  (reduce (fn [acc x] (+ (* acc 10) x)) 0 xs))

(defn answer-part-1 [parsed-input]
  (input->serial-number (find-serial parsed-input true [])))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 99893999291967])


;;;; Part 2

(defn answer-part-2 [parsed-input]
  (input->serial-number (find-serial parsed-input false [])))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 34171911181211])
