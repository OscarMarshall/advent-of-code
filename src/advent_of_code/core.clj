(ns advent-of-code.core
  (:require [clojure.string :as str]))

(def last-parse (atom {}))

(def last-answer (atom {}))

(defn current-parsed-input
  ([] (current-parsed-input :input))
  ([title] (@last-parse title)))

(defn current-answer
  ([part] (current-answer part :input))
  ([part title] (get-in @last-answer [part title])))

(defn get-input
  ([current-file] (get-input current-file :input))
  ([current-file type]
   (slurp (str/replace current-file
                       #"[a-z]?\.clj$"
                       (str "_input"
                            (if (not= type :input)
                              (str "_" (str/replace (name type) \- \_))
                              "")
                            ".txt")))))

(def function->symbol {= '=, not= 'not=, < '<, > '>})

(defn check-assertions [result assertions]
  (doseq [assertion assertions]
    (let [[op value] (if (vector? assertion) assertion [= assertion])]
      (assert (op result value)
              (str "(not (" (function->symbol op) " " result " " value "))")))))

(defn print-section [part parse-input answer current-file [title & assertions]]
  (println "###" title)
    (let [input        (get-input current-file title)
          parsed-input (parse-input input)
          result       (time (answer parsed-input))]
      (swap! last-parse assoc title parsed-input)
      (swap! last-answer assoc-in [part title] result)
      (prn result)
      (println)
      (check-assertions result assertions)))

(defn part
  {:style/indent [1 3]}
  [part parse-input answer current-file & sections]
  (println "## Part" part)
  (run! #(print-section part parse-input answer current-file %) sections)
  (current-answer part))
