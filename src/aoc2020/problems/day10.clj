(ns aoc2020.problems.day10
  (:require [aoc2020.util :refer [filter-count
                                  get-input]]
            [clojure.string :refer [split-lines]]))

(def input (get-input))

(defn parse-input [input] (map #(Integer. %) (split-lines input)))

(defn add-outlet&device [adapters]
  (conj adapters
        0
        (+ (apply max adapters) 3)))

(defn star1
  ([] (star1 input))
  ([input]
   (let [adapters (sort (add-outlet&device (parse-input input))) 
         diffs    (map - adapters (drop 1 adapters))]
     (* (filter-count #(= -1 %) diffs)
        (filter-count #(= -3 %) diffs)))))

(defn star2
  ([] (star2 input))
  ([input]
   nil))
