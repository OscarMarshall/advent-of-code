(ns advent-of-code.core
  (:require [clojure.datafy :refer [datafy]]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :as test]
            [hickory.convert :refer [hickory-to-hiccup]]
            [hickory.core :as hickory]
            [hickory.select :as select]
            [org.httpkit.client :as http])
  (:import (java.io File)))

(set! *warn-on-reflection* true)

(defonce session-cookie (atom nil))
(defonce report (atom nil))
(defonce state (atom nil))
(declare render-report)
(add-watch state :update-report (fn [_ _ _ state]
                                  (reset! report (render-report state))))

(defn- key->section-title [key]
  (string/join " "
               (sequence (comp (partition-by #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})
                               (map (partial apply str))
                               (mapcat #(string/split % #"-"))
                               (map string/capitalize))
                         (name key))))

(defn- render-input-section [{:as state, :keys [parse-fn]} input-key]
  (let [raw    (get-in state [:inputs input-key])
        start  (System/nanoTime)
        parsed (try
                 (when parse-fn (parse-fn raw))
                 (catch Exception exception
                   exception))
        end    (System/nanoTime)]
    [:section
     (when parse-fn
       [:div {:style {:float :right}}
        [:portal.viewer/duration-ns (- end start)]])
     [:h4 (key->section-title input-key)]
     [:div {:style {:display :grid, :grid-template-columns "50fr 50fr", :gap "12px"}}
      [:details {:style {:overflow :scroll}}
       [:summary [:b "Raw"]]
       [:portal.viewer/text raw]]
      (when parse-fn
        [:details {:style {:overflow :scroll}}
         [:summary [:b "Parsed"]]
         (if (isa? parsed Exception)
           [:portal.viewer/ex (datafy parsed)]
           [:portal.viewer/inspector parsed])])]]))

(defn- render-input-sections [{:as state, :keys [inputs]}]
  (into [:section
         [:h3 "Input"]]
        (map #(render-input-section state %))
        (sort (keys inputs))))

(defn- render-part-section [state part-key]
  (when-some [{:keys [description outputs]} (state part-key)]
    (into [:section
           [:h3 (key->section-title part-key)]
           [:details
            [:summary [:b "Description"]]
            description]
           (when outputs
             [:h4 "Results"])]
          (mapcat (fn [[input-key {:keys [result duration-ns test-reports]}]]
                    (into [[:div {:style {:float :right}}
                            [:portal.viewer/duration-ns duration-ns]]
                           [:h5 (key->section-title input-key)]
                           (if (isa? result Exception)
                             [:portal.viewer/ex (datafy result)]
                             [:portal.viewer/inspector result])]
                          (map (fn [test-report]
                                 [:portal.viewer/test-report test-report]))
                          test-reports)))
          outputs)))

(defn- render-part-sections [state]
  (into [:div]
        (map (fn [part]
               (let [part-key (keyword (str "part" part))]
                 (render-part-section state part-key))))
        [1 2]))

(defn- render-report [{:as state, :keys [title year]}]
  ^{:portal.viewer/default :portal.viewer/hiccup}
  [:main
   [:h1 (str "Advent of Code " year)]
   [:h2 (hickory-to-hiccup title)]
   (render-input-sections state)
   (render-part-sections state)])

(defn- input-filename ^String [year day type]
  (format "src/advent_of_code/year_%d/day_%02d_input%s.txt"
          year
          day
          (if (= type :puzzle) "" (str "_" (name type)))))

(defn- ensure-session-cookie []
  (when-not @session-cookie
    (throw (Exception. "Session cookie isn't set"))))

(defn- day-url [year day]
  (format "https://adventofcode.com/%d/day/%d" year day))

(defn- headers []
  {"Cookie" (str "session=" @session-cookie)})

(defn- fetch-day-description [year day]
  (let [html         (:body @(http/get (day-url year day) {:headers (headers)}))
        descriptions (select/select (select/class "day-desc")
                                    (hickory/as-hickory (hickory/parse html)))
        title        (-> descriptions
                         (get-in [0 :content 0 :content 0])
                         (->> (re-matches #"--- (.*) ---"))
                         (nth 1))]
    (cons title (map (comp hickory-to-hiccup #(update % :content subvec 1))
                     descriptions))))

(defn- fetch-puzzle-input! [year day]
  (ensure-session-cookie)
  (spit (input-filename year day :puzzle)
        (:body @(http/get (str (day-url year day) "/input")
                          {:headers (headers)}))))

(defn- ensure-puzzle-input! [year day]
  (when-not (.exists (io/file (input-filename year day :puzzle)))
    (fetch-puzzle-input! year day)))

(defn get-inputs [year day]
  (into {}
        (keep (fn [^File file]
                (let [filename (.getName file)]
                  (when (string/starts-with? filename (format "day_%02d_input"
                                                              day))
                    [(-> filename
                         (->> (re-find #"input_(.*)\.txt"))
                         (nth 1)
                         keyword
                         (or :puzzle))
                     (slurp file)]))))
        (file-seq (io/file (format "src/advent_of_code/year_%d" year)))))

(defn set-date! [year day]
  (ensure-puzzle-input! year day)
  (reset! state
          (let [[title part1-description part2-description]
                (fetch-day-description year day)]
            {:year   year
             :day    day
             :title  title
             :inputs (get-inputs year day)
             :part1  {:description part1-description}
             :part2  (when part2-description
                       {:description part2-description})})))

(defn set-parse-fn! [parse-fn]
  (swap! state assoc :parse-fn parse-fn))

(defn- calculate-output [input parse-fn answer-fn test-fn]
  (let [start  (System/nanoTime)
        output (try
                 (answer-fn (parse-fn input))
                 (catch Exception exception
                   exception))
        end    (System/nanoTime)]
    {:result       output
     :duration-ns  (- end start)
     :test-reports (let [reports (atom [])]
                     (with-redefs [test/do-report (partial swap! reports conj)]
                       (test-fn output))
                     @reports)}))

(defn set-answer-fn!* [part answer-fn & outputs]
  (swap! state (fn [{:as state, :keys [inputs parse-fn]}]
                 (update state
                         (keyword (str "part" part))
                         assoc
                         :outputs
                         (into {}
                               (map (fn [[input-key test-fn]]
                                      [input-key
                                       (calculate-output (inputs input-key)
                                                         parse-fn
                                                         answer-fn
                                                         test-fn)]))
                               outputs)))))

(defmacro set-answer-fn! {:style/indent [2]} [part answer-fn & outputs]
  `(set-answer-fn!* ~part
                    ~answer-fn
                    ~@(mapv (fn [[input-key & constraints]]
                              [input-key
                               `(fn [~'output]
                                  ~@(map (fn [x]
                                           (if (vector? x)
                                             (let [[op x] x]
                                               `(test/is (~op ~'output ~x)))
                                             `(test/is (~'= ~'output ~x))))
                                         constraints))])
                            outputs)))
