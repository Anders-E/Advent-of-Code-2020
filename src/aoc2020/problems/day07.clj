(ns aoc2020.problems.day07
  (:require [aoc2020.util :refer [filter-count
                                  get-input
                                  in?
                                  map-values
                                  p]]
            [clojure.string :refer [split-lines]]))

(def input (get-input 7))

(defn parse-line [line]
  (let [bag      (first (re-find #"^(\w+ \w+)" line))
        contains (->> (re-seq #"(\d+) (\w+ \w+)" line)
                      (map (comp vec
                                 reverse
                                 (p drop 1)))
                      (into {})
                      (map-values #(Integer. %)))]
    {bag contains}))

(defn parse-input [input]
  (->> (split-lines input)
       (map parse-line)
       (apply merge)))

(defn contains-shiny-golden? [rules bag]
  (let [bags (keys (get rules bag))]
    (or (in? "shiny gold" bags)
        (some (p contains-shiny-golden? rules) bags))))

(defn count-bags [rules bag]
  (let [contents          (get rules bag)
        count-nested-bags (fn [[bag n]] (+ (* (count-bags rules bag)
                                              n)
                                           n))]
    (apply + (map count-nested-bags contents))))

(defn star1
  ([] (star1 input))
  ([input]
   (let [rules (parse-input input)]
     (->> (keys rules)
          (map (p contains-shiny-golden? rules))
          (filter-count #(= % true))))))

(defn star2
  ([] (star2 input))
  ([input]
   (count-bags (parse-input input) "shiny gold")))
