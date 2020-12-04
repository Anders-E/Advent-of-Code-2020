(ns aoc2020.problems.day03
  (:require [aoc2020.util :refer [filter-count
                                  get-input
                                  p]]
            [clojure.string :refer [split-lines]]))

(def input (get-input 3))

(defn getget [coll x y] (get (get coll y) x))

(defn slope [h [x-step y-step]]
  (map vector
       (iterate (p + x-step) 0)
       (take h (iterate (p + y-step) 0))))

(defn slopes [h]
  (map (p slope h) '([1 1] [3 1] [5 1] [7 1] [1 2])))

(defn trees [tree-map slope]
  (let [w (count (first tree-map))]
    (->> slope
         (map (fn [[x y]] (getget tree-map (rem x w) y)))
         (filter-count (p = \#)))))

(defn star1
  ([] (star1 input))
  ([input]
   (let [tree-map (split-lines input)
         h        (count tree-map)]
     (trees tree-map (nth (slopes h) 1)))))

(defn star2
  ([] (star2 input))
  ([input]
   (let [tree-map (split-lines input)
         h        (count tree-map)]
     (apply * (map (p trees tree-map)
                   (slopes h))))))
