(ns advent-of-code.year-2019.intcode-computer
  (:require [clojure.edn :as edn]
            [clojure.string :as string]
            [medley.core :as medley]))

(defn interrupt [state type]
  (assoc state :interrupt type))

(defn arithmetic-opcode [opcode-name f]
  {:name     opcode-name
   :size     4
   :function (fn [state {a :value} {b :value} {:keys [store]}]
               (store state (f a b)))})

(defn conditional-opcode [opcode-name f]
  (arithmetic-opcode opcode-name (fn [a b] (if (f a b) 1 0))))

(def opcodes
  {;; add
   1  (arithmetic-opcode "ADDI" +')
   ;; multiply
   2  (arithmetic-opcode "MULT" *')
   ;; input
   3  {:name     "IN"
       :size     2
       :function (fn [{:as state, :keys [inputs]} {:keys [store]}]
                   (let [input (peek inputs)]
                     (if input
                       (-> state
                           (store input)
                           (update :inputs pop))
                       (interrupt state :input))))}
   ;; output
   4  {:name     "OUT"
       :size     2
       :function (fn [state {:keys [value]}]
                   (update state :outputs conj value))}
   ;; jump-if-true
   5  {:name     "JIFT"
       :size     3
       :function (fn [state {condition :value} {destination :value}]
                   (cond-> state
                     (not (zero? condition)) (assoc :instruction-pointer
                                                    destination)))}
   ;; jump-if-false
   6  {:name     "JIFF"
       :size     3
       :function (fn [state {condition :value} {destination :value}]
                   (cond-> state
                     (zero? condition) (assoc :instruction-pointer
                                              destination)))}
   ;; less than
   7  (conditional-opcode "LT" <)
   ;; equals
   8  (conditional-opcode "EQ" =)
   9  {:size 2, :function (fn [state {:keys [value]}]
                            (update state :relative-base + value))}
   99 {:size 1, :function #(interrupt % :halt)}})

(defn parse-program [string]
  (mapv parse-long (string/split (string/trim string) #",")))

(defn parse-intcode [intcode]
  [(rem intcode 100)
   (->> (iterate (fn [[x]] [(quot x 10) (rem x 10)]) [(quot intcode 100)])
        (drop 1)
        (map second))])

(defn get-parameters [{:keys [instruction-pointer memory relative-base]}
                      parameter-count
                      modes]
  (map (fn [parameter mode]
         (let [value (cond-> (memory (+ instruction-pointer 1 parameter))
                       (= mode 2) (+ relative-base))]
           {:address (when (not= mode 1) value)
            :value   (cond-> value (not= mode 1) (memory 0))
            :store   #(assoc-in %1 [:memory value] %2)}))
       (range parameter-count)
       modes))

(defn step [{:as state, :keys [memory instruction-pointer]}]
  (let [intcode                 (memory instruction-pointer)
        [opcode modes]          (parse-intcode intcode)
        {:keys [size function]} (opcodes opcode)
        parameters              (get-parameters state (dec size) modes)
        next-state              (apply function state parameters)]
    (cond-> next-state
      (and (not (:interrupt next-state))
           (= instruction-pointer (:instruction-pointer next-state)))
      (update :instruction-pointer + size))))

(defn load-program [program]
  {:memory              (zipmap (range) program)
   :instruction-pointer 0
   :relative-base       0
   :inputs              (medley/queue)
   :outputs             (medley/queue)})

(defn continue [state & inputs]
  (-> state
      (dissoc :interrupt)
      (update :inputs into inputs)
      (->> (iterate step)
           (medley/find-first :interrupt))))

(defn start [program & inputs]
  (apply continue (load-program program) inputs))

(defn halted? [{:keys [interrupt]}] (= interrupt :halt))

(defn read-output [{:as state, :keys [outputs]}]
  [(peek outputs) (update state :outputs pop)])
