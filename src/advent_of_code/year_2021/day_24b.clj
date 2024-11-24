(ns advent-of-code.year-2021.day-24b
  (:require [advent-of-code.core :as core]
            [clojure.core.logic :as logic]
            [clojure.core.logic.fd :as fd]
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

(defn translate-sub-instructions [instructions]
  (map (fn [{:as instruction, :keys [op b]}]
         (cond-> instruction
           (and (= op :add) (number? b) (neg? b)) (assoc :op :sub, :b (- b))))
       instructions))

(core/set-parse-fn! (comp translate-sub-instructions parse-input))


;;;; Part 1

(def input-domain (apply fd/domain (range 1 10)))
(def register-domain (fd/interval Integer/MAX_VALUE))
(def instruction-domain (apply fd/domain (range (inc 26))))

(logic/defne instruction-locations
  [instruction start-state a b result end-state]
  ([{:op _, :a a-register, :b b-register}
    {:w w, :x x, :y y, :z z, :inputs _}
    _
    _
    _
    {:w end-w, :x end-x, :y end-y, :z end-z, :inputs _}]
   (logic/matche [a-register]
     ([:w]
      (logic/== a w)
      (logic/== end-w result)
      (logic/== end-x x)
      (logic/== end-y y)
      (logic/== end-z z))
     ([:x]
      (logic/== a x)
      (logic/== end-w w)
      (logic/== end-x result)
      (logic/== end-y y)
      (logic/== end-z z))
     ([:y]
      (logic/== a y)
      (logic/== end-w w)
      (logic/== end-x x)
      (logic/== end-y result)
      (logic/== end-z z))
     ([:z]
      (logic/== a z)
      (logic/== end-w w)
      (logic/== end-x x)
      (logic/== end-y y)
      (logic/== end-z result)))
   (logic/matche [b-register]
     ([:w] (logic/== b w))
     ([:x] (logic/== b x))
     ([:y] (logic/== b y))
     ([:z] (logic/== b z))
     ([b]
      (logic/!= b-register :w)
      (logic/!= b-register :x)
      (logic/!= b-register :y)
      (logic/!= b-register :z)))))

(logic/defne execo [start-state instructions end-state]
  ([end-state () _])
  ([{:w w, :x x, :y y, :z z, :inputs inputs}
    [instruction . more-instructions]
    _]
   (logic/fresh [a b result mid-state op]
     (fd/in a result register-domain)
     (instruction-locations instruction start-state a b result mid-state)
     (logic/featurec instruction {:op op})
     (logic/matche [op]
       ([:inp]
        (logic/== b nil)
        (fd/in result input-domain)
        (logic/firsto inputs result))
       ([:add] (fd/+ a b result))
       ([:sub] (fd/- a b result))
       ([:mul] (fd/* a b result))
       ([:div] (logic/fresh [rem c]
                 (fd/< rem b)
                 (fd/- a rem c)
                 (fd/quot c b result)))
       ([:mod] (logic/fresh [quot c]
                 (fd/< result b)
                 (fd/- a result c)
                 (fd/quot c b quot)))
       ([:eql] (logic/conde
                 [(fd/== a b) (logic/== result 1)]
                 [(fd/!= a b) (logic/== result 0)])))
     (logic/matche [mid-state]
       ([{:w _, :x _, :y _, :z _, :inputs mid-inputs}]
        (logic/conde
         [(logic/== op :inp) (logic/resto inputs mid-inputs)]
         [(logic/!= op :inp) (logic/== mid-inputs inputs)])))
     (execo mid-state more-instructions end-state))))

(defn run-program [program prefix]
  (first (logic/run 1 [inputs]
           (logic/fresh [suffix end-w end-x end-y]
             (logic/appendo prefix suffix inputs)
             (execo {:w 0, :x 0, :y 0, :z 0, :inputs inputs}
                    program
                    {:w end-w, :x end-x, :y end-y, :z 0, :inputs ()})))))

(defn find-serial [program largest prefix]
  (if (= (count prefix) 14)
    prefix
    (->> (cond-> (range 1 10) largest reverse)
         (map (partial conj prefix))
         (some (fn [prefix]
                 (when (run-program program prefix)
                   (find-serial program largest prefix)))))))

(defn inputs->serial-number [xs]
  (reduce (fn [acc x] (+ (* acc 10) x)) 0 xs))

(defn answer-part-1 [parsed-input]
  (inputs->serial-number (find-serial parsed-input true [])))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 99893999291967])


;;;; Part 2

(defn answer-part-2 [parsed-input]
  (inputs->serial-number (find-serial parsed-input false [])))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 34171911181211])
