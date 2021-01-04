(ns advent-of-code-2020.core
  (:require [clojure.string :as string]))

(defmacro get-input []
  `(slurp ~(string/replace *file* #"[a-z]?.clj" "_input.txt")))
