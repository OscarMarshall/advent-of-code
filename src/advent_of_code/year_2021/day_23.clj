(ns advent-of-code.year-2021.day-23
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(def input (core/get-input))

(def rooms [:room-a :room-b :room-c :room-d])
(def hallways
  [:hallway0 :hallway1 :hallway2 :hallway3 :hallway4 :hallway5 :hallway6])
(def location-keys (into rooms hallways))

(defn parse-input [input]
  (let [amphipods (map first (re-seq #"[A-Z]" input))]
    (zipmap rooms
            (map (fn [i] (take-nth 4 (drop i amphipods))) (range 4)))))

(def parsed-input (parse-input input))


;;; Part 1
;;; ============================================================================

(def location-graph
  {:hallway0 {:hallway1 1}
   :hallway1 {:hallway0 1
              :hallway2 2
              :room-a   2}
   :room-a   {:hallway1 2
              :hallway2 2}
   :hallway2 {:hallway1 2
              :hallway3 2
              :room-a   2
              :room-b   2}
   :room-b   {:hallway2 2
              :hallway3 2}
   :hallway3 {:hallway2 2
              :hallway4 2
              :room-b   2
              :room-c   2}
   :room-c   {:hallway3 2
              :hallway4 2}
   :hallway4 {:hallway3 2
              :hallway5 2
              :room-c   2
              :room-d   2}
   :room-d   {:hallway4 2
              :hallway5 2}
   :hallway5 {:hallway4 2
              :hallway6 1
              :room-d   2}
   :hallway6 {:hallway5 1}})

(def movement-cost {\A 1, \B 10, \C 100, \D 1000})

(defn destination [c] (keyword (str "room-" (string/lower-case (str c)))))

(defn room-location? [location]
  (string/starts-with? (name location) "room-"))

(def hallway-location? (complement room-location?))

(defn organized? [locations]
  (and (every? (comp empty? locations) hallways)
       (every? (fn [room]
                 (let [amphipods (locations room)]
                   (and (apply = amphipods)
                        (= (destination (first amphipods)) room))))
               rooms)))

(defn reachable-locations [locations location]
  (loop [queue (sorted-set [0 location]), seen #{}, result {}]
    (if (empty? queue)  result
        (let [[score location :as k] (first queue)
              queue                  (disj queue k)]
          (if (seen location)
            (recur queue seen result)
            (recur (into queue
                         (comp (filter (comp (some-fn room-location?
                                                      (comp empty? locations))
                                             key))
                               (map (juxt (comp (partial + score) val) key)))
                         (location-graph location))
                   (conj seen location)
                   (assoc result location score)))))))

(defn parsed-input->locations [parsed-input]
  (into parsed-input (map (juxt identity (constantly ()))) hallways))

(defn move-amphipod [locations from to]
  (-> locations
      (update from rest)
      (update to conj (first (locations from)))))

(defn triangle-number [n] (/ (* n (+ n 1)) 2))

(defn min-steps [locations]
  (let [score (apply +
                     (concat (map (partial * (->> locations
                                                  vals
                                                  (map count)
                                                  (apply max)
                                                  dec
                                                  triangle-number))
                                  (vals movement-cost))
                             (flatten (map (partial map-indexed
                                                    (fn [i t]
                                                      (* i (movement-cost t))))
                                           (vals locations)))))]
    (loop [queue (sorted-map [score (hash locations)] [locations [locations]])
           seen  #{}]
      (let [[[score :as k] [locations history]] (first queue)
            queue                               (dissoc queue k)]
        (if (organized? locations)
          score
          (recur
           (cond-> queue
             (not (seen locations))
             (into
              (comp
               (filter (comp seq val))
               (mapcat
                (fn [[from [amphipod]]]
                  (let [destination       (destination amphipod)
                        destination-ready (every? #{amphipod}
                                                  (locations destination))]
                    (when (or (not= from destination) (not destination-ready))
                      (eduction
                       (filter (comp (some-fn (if (room-location? from)
                                                hallway-location?
                                                (constantly false))
                                              (if destination-ready
                                                #{destination}
                                                (constantly false)))
                                     key))
                       (map (fn [[to steps]]
                              (let [locations (move-amphipod locations from to)]
                                [[(+ score (* (movement-cost amphipod) steps))
                                  (hash locations)]
                                 [locations (conj history locations)]])))
                       (reachable-locations locations from)))))))
              locations))
           (conj seen locations)))))))

(defn answer-part-1 [parsed-input]
  (min-steps (parsed-input->locations parsed-input)))

(def part-1-answer (answer-part-1 parsed-input))

(assert (= part-1-answer 18282))


;;; Part 2
;;; ============================================================================

(def new-input
  {:room-a '(\D \D)
   :room-b '(\C \B)
   :room-c '(\B \A)
   :room-d '(\A \C)})

(defn answer-part-2 [parsed-input]
  (->> parsed-input
       (into {} (map (fn [[k [a b]]] [k `(~a ~@(new-input k) ~b)])))
       parsed-input->locations
       min-steps))

(def part-2-answer (answer-part-2 parsed-input))

(assert (= part-2-answer 50132))
