(ns advent-of-code.year-2020.day-16
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2020 16)


;;;; Parse

(defn parse-field [line]
  (let [[_ name a1 z1 a2 z2] (re-matches #"(.+): (\d+)-(\d+) or (\d+)-(\d+)"
                                         line)
        [a1 z1 a2 z2]        (map parse-long [a1 z1 a2 z2])]
    [name (fn [x] (or (<= a1 x z1) (<= a2 x z2)))]))

(defn parse-ticket [line] (map parse-long (string/split line #",")))

(defn parse-input [input]
  (let [[fields my-ticket nearby-tickets] (string/split input #"\n\n")]
    {:fields         (into {} (map parse-field) (string/split-lines fields))
     :my-ticket      (parse-ticket (second (string/split-lines my-ticket)))
     :nearby-tickets (->> nearby-tickets
                          string/split-lines
                          rest
                          (map parse-ticket))}))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn answer-part-1 [parsed-input]
  (let [{:keys [fields nearby-tickets]} parsed-input]
    (transduce (remove (apply some-fn (vals fields)))
               +
               (flatten nearby-tickets))))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 25059])


;;;; Part 2

(def determine-fields
  (memoize
   (fn [remaining-fields [values & field-values]]
     (if values
       (->> remaining-fields
            (filter (fn [[_ possible?]] (possible? values)))
            (map key)
            (keep (fn [field]
                    (some->> (determine-fields (dissoc remaining-fields field)
                                               field-values)
                             (cons field))))
            first)
       ()))))

(defn answer-part-2 [parsed-input]
  (let [{:keys [fields nearby-tickets my-ticket]} parsed-input

        nearby-tickets
        (filter (partial every? (apply some-fn (vals fields))) nearby-tickets)

        field-values (apply map hash-set nearby-tickets)

        field-order
        (determine-fields (into {}
                                (map (fn [[field valid?]]
                                       [field
                                        (memoize (partial every? valid?))]))
                                fields)
                          field-values)]
    (transduce (comp (filter (comp #(string/starts-with? % "departure") key))
                     (map val))
               *
               (zipmap field-order my-ticket))))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 3253972369789])
