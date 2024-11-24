(ns advent-of-code.year-2020.day-22
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2020 22)


;;;; Parse

(defn queue
  ([] clojure.lang.PersistentQueue/EMPTY)
  ([coll] (into clojure.lang.PersistentQueue/EMPTY coll)))

(defn parse-input [input]
  (into []
        (comp (map string/split-lines)
              (map rest)
              (map (partial into (queue) (map parse-long))))
        (string/split input #"\n\n")))

(core/set-parse-fn! parse-input)


;;;; Part 1

(defn calculate-score [deck]
  (apply + (map * (reverse deck) (rest (range)))))

(defn answer-part-1 [parsed-input]
  (let [[_ deck] (loop [player1 (first parsed-input)
                        player2 (second parsed-input)]
                   (cond
                     (empty? player2) [:player1 player1]
                     (empty? player1) [:player2 player2]
                     :else            (let [player1-card (peek player1)
                                            player2-card (peek player2)
                                            player1      (pop player1)
                                            player2      (pop player2)]
                                        (if (> player1-card player2-card)
                                          (recur (conj player1
                                                       player1-card
                                                       player2-card)
                                                 player2)
                                          (recur player1
                                                 (conj player2
                                                       player2-card
                                                       player1-card))))))]
    (calculate-score deck)))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 31957])


;;;; Part 2

(defn play-game
  ([player1 player2] (play-game player1 player2 #{}))
  ([player1 player2 previous-configurations]
   (if (contains? previous-configurations [player1 player2])
     [:player1 player1]
     (let [previous-configurations (conj previous-configurations
                                         [player1 player2])]
       (cond
         (empty? player2) [:player1 player1]
         (empty? player1) [:player2 player2]

         :else
         (let [player1-card (peek player1)
               player2-card (peek player2)
               player1      (pop player1)
               player2      (pop player2)
               winner       (cond
                              (and (>= (count player1) player1-card)
                                   (>= (count player2) player2-card))
                              (first (play-game (queue (take player1-card
                                                             player1))
                                                (queue (take player2-card
                                                             player2))))

                              (> player1-card player2-card)
                              :player1

                              :else
                              :player2)]
           (if (= winner :player1)
             (recur (conj player1 player1-card player2-card)
                    player2
                    previous-configurations)
             (recur player1
                    (conj player2 player2-card player1-card)
                    previous-configurations))))))))

(defn answer-part-2 [parsed-input]
  (calculate-score (second (apply play-game parsed-input))))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle 33212])
