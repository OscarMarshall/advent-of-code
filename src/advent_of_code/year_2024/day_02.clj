(ns advent-of-code.year-2024.day-02
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]
            [medley.core :as medley]
            [clojure.core.logic :as logic]
            [clojure.core.logic.fd :as fd]))

(set! *warn-on-reflection* true)

(core/set-date! 2024 2)


;;;; Parse

(defn parse-input [input]
  (map (fn [line] (map parse-long (string/split line #" ")))
       (string/split-lines input)))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn safe-report? [report]
  (let [differences (map - report (rest report))]
    (or (every? #{1 2 3} differences) (every? #{-1 -2 -3} differences))))

(defn answer-part-1 [reports]
  (count (filter safe-report? reports)))

(core/set-answer-fn! 1 answer-part-1
  [:sample1 2]
  [:puzzle 639])


;;;; Part 2

(logic/defne safe-reporto [xs safe]
  ([[] true])
  ([[_] true])
  ([[x y . more] _]
   (logic/fresh [z xxs]
     (logic/everyg #(fd/in % (fd/interval 99)) [x y z])
     (logic/conde
       [(fd/- y x z)
        (fd/<= 1 z)
        (fd/<= z 3)
        (logic/conso y more xxs)
        (safe-reporto xxs safe)]
       [(logic/conde
          [(fd/> x y)]
          [(fd/== x y)]
          [(fd/- y x z)
           (fd/> z 3)])
        (logic/== safe false)]))))

(logic/defne safe-report-dampenedo [xs safe]
  ([[] true])
  ([[_] true])
  ([[x y . more] _]
   (logic/fresh [z xxs]
     (logic/everyg #(fd/in % (fd/interval 99)) [x y z])
     (logic/conde
       [(fd/- y x z)
        (fd/<= 1 z)
        (fd/<= z 3)
        (logic/conso y more xxs)
        (safe-report-dampenedo xxs safe)]
       [(logic/conso y more xxs)
        (safe-reporto xxs safe)]
       [(logic/conso x more xxs)
        (safe-reporto xxs safe)]))))

(comment
  (some identity (logic/run* [x] (safe-report-dampenedo [1 5] x)))
  )

(defn problem-dampened-safe-report? [report]
  #_(some safe-report?
        (cons report
              (map #(medley/remove-nth % report) (range (count report)))))
  (or (some identity (logic/run* [x] (safe-report-dampenedo report x)))
      (some identity (logic/run* [x]
                       (safe-report-dampenedo (reverse report) x)))))

(defn answer-part-2 [reports]
  (count (filter problem-dampened-safe-report? reports)))

(core/set-answer-fn! 2 answer-part-2
  [:sample1 4]
  [:puzzle [> 672] 674])
