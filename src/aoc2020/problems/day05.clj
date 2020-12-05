(ns aoc2020.problems.day05
  (:require [aoc2020.util :refer [get-input]]
            [clojure.set :refer [difference]]
            [clojure.string :refer [split-lines]]))

(def input (get-input 5))

(defn partitioning->bin [partitioning]
  (Integer/parseInt (apply str (map {\F 0 \B 1 \R 1 \L 0} partitioning)) 2))

(defn seat-id [partitioning]
  (let [bin (partitioning->bin partitioning)
        row (bit-shift-right bin 3)
        col (bit-and 7 bin)]
    (+ (* row 8) col)))

(defn star1
  ([] (star1 input))
  ([input]
   (->> (split-lines input)
        (map seat-id)
        (apply max))))

(defn star2
  ([] (star2 input))
  ([input]
   (let [ids (set (map seat-id (split-lines input)))
         min (apply min ids)
         max (apply max ids)]
     (first (difference (set (range min max)) ids)))))
