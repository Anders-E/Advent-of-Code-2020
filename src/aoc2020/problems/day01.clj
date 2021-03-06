(ns aoc2020.problems.day01
  (:require [aoc2020.util :refer [get-input]]
            [clojure.string :refer [split-lines]]))

(def input (get-input))

(defn parse-input [input] (map #(Integer. %) (split-lines input)))

(defn pairs [seq]
  (for [a seq
        b seq]
    [a b]))

(defn triplets [seq]
  (for [a seq
        b seq
        c seq]
    [a b c]))

(defn fix-expense-report [nums chunks]
  (->> (chunks nums)
       (filter #(= (apply + %)
                   2020))
       (first)
       (apply *)))

(defn star1
  ([] (star1 input))
  ([input]
   (fix-expense-report (parse-input input) pairs)))

(defn star2
  ([] (star2 input))
  ([input]
   (fix-expense-report (parse-input input) triplets)))
