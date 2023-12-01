(ns hooks.db-rel
  (:require [clj-kondo.hooks-api :as api]))

(defn db-rel [{:keys [node]}]
  (let [[name & args] (rest (:children node))]
    (when-not name (throw (ex-info "No name provided" {})))
    {:node (api/list-node (list (api/token-node 'defn)
                                name
                                (api/vector-node [args])))}))
