(ns aoc2020.problems.day02
  (:require [aoc2020.util :refer [get-input
                                  p]]
            [clojure.string :refer [split-lines]]))

(def input (get-input 2))

(defn split-line [line] (rest (re-matches #"(\d+)-(\d+) (\w): (\w+)" line)))

(defn parse-input [input] (map split-line (split-lines input)))

(defn valid-password1 [min max char password]
  (let [min        (Integer. min)
        max        (Integer. max)
        char       (get char 0)
        charcounts (frequencies password)
        count      (get charcounts char 0)]
    (<= min count max)))

(defn valid-password2 [min max char password]
  (let [min        (Integer. min)
        max        (Integer. max)
        char       (nth char 0)
        a          (nth password (dec min))
        b          (nth password (dec max))]
    (and (or (= char a)
             (= char b))
         (not= a b))))

(defn validate-passwords [input policy]
  (->> (parse-input input)
       (map (p apply policy))
       (filter #(= true %))
       (count)))

(defn star1
  ([]
   (star1 input))
  ([input]
   (validate-passwords input valid-password1)))

(defn star2
  ([]
   (star2 input))
  ([input]
   (validate-passwords input valid-password2)))
