(ns advent-of-code.year-2020.day-16
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(def input (core/get-input *file*))

(defn parse-field [line]
  (let [[_ name a1 z1 a2 z2] (re-matches #"(.+): (\d+)-(\d+) or (\d+)-(\d+)"
                                         line)
        [a1 z1 a2 z2]        (map #(Long/parseLong %) [a1 z1 a2 z2])]
    [name (fn [x] (or (<= a1 x z1) (<= a2 x z2)))]))

(defn parse-ticket [line]
  (map #(Long/parseLong %) (string/split line #",")))


;;; Part 1
;;; ============================================================================

(defn parse-input [input]
  (let [[fields my-ticket nearby-tickets] (string/split input #"\n\n")]
    {:fields         (into {} (map parse-field) (string/split-lines fields))
     :my-ticket      (parse-ticket (second (string/split-lines my-ticket)))
     :nearby-tickets (->> nearby-tickets
                          string/split-lines
                          rest
                          (map parse-ticket))}))

(def parsed-input (parse-input input))

(defn answer-part-1 [parsed-input]
  (let [{:keys [fields nearby-tickets]} parsed-input]
    (transduce (remove (apply some-fn (vals fields)))
               +
               (flatten nearby-tickets))))

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 25059))


;;; Part 2
;;; ============================================================================

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

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer 3253972369789))
