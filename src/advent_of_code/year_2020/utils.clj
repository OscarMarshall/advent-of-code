(ns advent-of-code.year-2020.utils)

(defn conway-step-fn [neighbors becomes-active? stays-active?]
  (fn [active]
    (->> active
         (reduce (fn [active-neighbors posn]
                   (reduce (fn [active-neighbors id]
                             (update active-neighbors id (fnil inc 0)))
                           active-neighbors
                           (neighbors posn)))
                 (zipmap active (repeat 0)))
         (reduce (fn [next-active [id active-neighbors]]
                   (cond-> next-active
                     (if (active id)
                       (stays-active? active-neighbors)
                       (becomes-active? active-neighbors))
                     (conj id)))
                 #{}))))
