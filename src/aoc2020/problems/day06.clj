(ns aoc2020.problems.day06
  (:require [aoc2020.util :refer [get-input
                                  p]]
            [clojure.set :refer [intersection]]
            [clojure.string :refer [split
                                    split-lines]]))

(def input (get-input 6))

(defn star1
  ([] (star1 input))
  ([input]
   (->> (split input #"\n\n")
        (map (comp count
                   #(disj % \newline)
                   set))
        (apply +))))

(defn star2
  ([] (star2 input))
  ([input]
   (->> (split input #"\n\n")
        (map (comp count
                   (p reduce intersection)
                   (p map set)
                   split-lines))
        (apply +))))
