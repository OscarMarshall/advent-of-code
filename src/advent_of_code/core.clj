(ns advent-of-code.core
  (:require [clojure.string :as str]))

(defn get-input
  ([current-file] (get-input current-file nil))
  ([current-file type]
   (slurp (str/replace current-file
                       #"[a-z]?\.clj$"
                       (str "_input"
                            (if type
                              (str "_" (str/replace (name type) \- \_))
                              "")
                            ".txt")))))
