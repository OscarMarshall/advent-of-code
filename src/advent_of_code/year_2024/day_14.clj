(ns advent-of-code.year-2024.day-14
  (:require [advent-of-code.core :as core]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(core/set-date! 2024 14)


;;;; Parse

(defn parse-input [input]
  (eduction (comp (map rest)
                  (map (partial map parse-long))
                  (map (fn [[px py vx vy]]
                         {:position [px py], :velocity [vx vy]})))
            (re-seq #"p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)" input)))

(core/set-parse-fn! parse-input)


;;;; Part 1

(def width 101)

(def height 103)

(defn move-robot [{:keys [velocity], :as robot} seconds]
  (-> robot
      (update :position (partial mapv + (map (partial * seconds) velocity)))
      (update-in [:position 0] mod 101)
      (update-in [:position 1] mod 103)))

(defn answer-part-1 [robots]
  (let [robots (map #(move-robot % 100) robots)]
    (transduce (map count)
               *
               (for [[x-min x-max] [[0 (dec (quot width 2))]
                                    [(inc (quot width 2)) (dec width)]]
                     [y-min y-max] [[0 (dec (quot height 2))]
                                    [(inc (quot height 2)) (dec height)]]]
                 (filter (fn [{[x y] :position}]
                           (and (<= x-min x x-max) (<= y-min y y-max)))
                         robots)))))

(core/set-answer-fn! 1 answer-part-1
  [:puzzle 229069152])


;;;; Part 2

(def seconds 7383
  #_(+ 1222 (* 101 4)))

(comment
  414
  515

  482
  585

  (loop [x (iterate (partial + 101) 515)
         y (iterate (partial + 103) 585)]
    (cond
      (= (first x) (first y)) x
      (< (first x) (first y)) (recur (rest x) y)
      :else (recur x (rest y))))
  )

(defn area-string [robots]
  (let [positions (into #{} (map :position) robots)]
    (string/join "\n"
                 (map (fn [y]
                        (string/join (map (fn [x]
                                            (if (positions [x y]) "█" " "))
                                          (range width))))
                      (range height)))))

(defn answer-part-2 [robots]
  (area-string (map #(move-robot % seconds) robots)))

(core/set-answer-fn! 2 answer-part-2
  [:puzzle (str "                                    █                                          █                   █ \n"
                "                                          █                                                   █      \n"
                "                                                                                               █     \n"
                "                                            █                               █                        \n"
                "         █                       █                                                            █      \n"
                "                                               █                                              █      \n"
                "                                                                                                     \n"
                "                     ██                                                            █                 \n"
                "     █                                                                                               \n"
                "                    █                     █    █                                                     \n"
                "                    █      █                               █                                         \n"
                "    █                  █                                      █                                      \n"
                "                               █                                                                     \n"
                "                                                                                                     \n"
                "                                                                               █                     \n"
                "                                                                                                     \n"
                "                           █                  █                                                      \n"
                "                      █                █                                                             \n"
                "                                                          █                            █             \n"
                "                                                                                                     \n"
                "                             █                        █                              █               \n"
                "                                                        █                                            \n"
                "                         █                              █                                         █  \n"
                "                                                         █                                           \n"
                "     █                                                                                       █       \n"
                "                             █                                                                       \n"
                "                                                                                                     \n"
                "                                                                                      █            █ \n"
                "                            █                  █         █                                           \n"
                "               █     █              █  █                                                             \n"
                "                                                                                               █     \n"
                "                                    █         █                    █                                 \n"
                "█                                    █                             █           █                     \n"
                "  █                                                                                                  \n"
                "                                                                                       █             \n"
                "                                                              █                                      \n"
                "                                                                 █                                   \n"
                "                                               █                                                     \n"
                "                                                                                                     \n"
                "  █              █   █                                                                █              \n"
                "           █                       █                           █                                  █  \n"
                "                                  █                                                                  \n"
                "                                                                                                     \n"
                "                                                                                 █                   \n"
                "                                                                             █          █            \n"
                "                                                                  █                                  \n"
                "                                                                                                     \n"
                "                                                    ███████████████████████████████                  \n"
                " █  █     █                                         █                             █                  \n"
                "    █                          █                    █                             █                  \n"
                "█                                                   █                             █                  \n"
                "                                       █            █                             █                  \n"
                "                                                    █              █              █                  \n"
                "                         █                          █             ███             █ █                \n"
                "                                                █   █            █████            █  █               \n"
                "         █                       █                  █           ███████           █                  \n"
                "                █      █      █                     █          █████████          █                  \n"
                "     █                                              █            █████            █                  \n"
                "                                         █          █           ███████           █                  \n"
                "                                                    █          █████████          █                  \n"
                "                   █              █              █  █         ███████████         █                  \n"
                "                                          █         █        █████████████        █                  \n"
                "                                                    █          █████████          █                 █\n"
                "                                                    █         ███████████         █                  \n"
                "                                    █               █        █████████████        █                  \n"
                "                                                    █       ███████████████       █                  \n"
                "                  █                                 █      █████████████████      █                  \n"
                "                                                    █        █████████████        █               █  \n"
                "                                                    █       ███████████████       █                  \n"
                "                                     █              █      █████████████████      █                  \n"
                "                                                    █     ███████████████████     █  █               \n"
                "                                                    █    █████████████████████    █                  \n"
                "          █                                         █             ███             █                  \n"
                "                                                    █             ███             █                  \n"
                "                                                    █             ███             █                 █\n"
                "                                                    █                             █              █   \n"
                "                                                    █                             █         █  █     \n"
                "    █                                           █   █                             █                  \n"
                "                                                    █                             █                  \n"
                "                                                █   ███████████████████████████████                  \n"
                "                        █                      █                                                     \n"
                "                                █             █                                                      \n"
                "                                                                                                     \n"
                "    █                                                                                                \n"
                "                                  █                                                                  \n"
                "                                                                                                     \n"
                "                                                                              █                      \n"
                "                  █                                           █                                      \n"
                "                                                                                                     \n"
                "                                       █       █                                                     \n"
                "                 █                  █                                                                \n"
                "                                  █        █                                                         \n"
                "                                                                                                 █   \n"
                "                          █                                                                          \n"
                "                                                                                                     \n"
                "          █                           █                         █                                    \n"
                "                       █                                                                             \n"
                "                                                                                                     \n"
                "                                █                                █                                   \n"
                " █                                                                                                █  \n"
                "                                                         █              █                            \n"
                "                                         █                                          █                \n"
                "                                            █                            █                           ")])
