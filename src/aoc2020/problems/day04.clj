(ns aoc2020.problems.day04
  (:require [aoc2020.util :refer [filter-count
                                  get-input
                                  p]]
            [clojure.string :refer [split]]))

(def input (get-input 4))

(defn parse-input [input] 
  (->> (split input #"\n\n")
       (map #(split % #"\s"))
       (map (p map #(split % #":")))))

(def required-fields ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid" "cid"])

(defmulti valid-field (fn [[field _]] field))

(defmethod valid-field "byr"
  [[_ value]] (and (re-matches #"\d{4}" value)
                   (<= 1920 (Integer. value) 2002)))

(defmethod valid-field "iyr"
  [[_ value]] (and (re-matches #"\d{4}" value)
                   (<= 2010 (Integer. value) 2020)))

(defmethod valid-field "eyr"
  [[_ value]] (and (re-matches #"\d{4}" value)
                   (<= 2020 (Integer. value) 2030)))

(defmethod valid-field "hgt"
  [[_ value]] (let [[_ num unit] (re-matches #"(\d+)(cm|in)" value)]
                (or (and (= unit "cm")
                         (<= 150 (Integer. num) 193))
                    (and (= unit "in")
                         (<= 59 (Integer. num) 76)))))

(defmethod valid-field "hcl"
  [[_ value]] (boolean (re-matches #"#[0-9a-f]{6}" value)))

(defmethod valid-field "ecl"
  [[_ value]] (boolean (re-matches #"(amb|blu|brn|gry|grn|hzl|oth)" value)))

(defmethod valid-field "pid"
  [[_ value]] (boolean (re-matches #"\d{9}" value)))

(defmethod valid-field "cid" [_] true)

(defn star1
  ([] (star1 input))
  ([input]
   (->> (parse-input input)
        (map #(conj % ["cid" ""]))
        (filter-count #(every? (into {} %) required-fields)))))

(defn star2
  ([] (star2 input))
  ([input]
   (->> (parse-input input)
        (map #(conj % ["cid" ""]))
        (filter-count #(and (every? (into {} %) required-fields)
                            (every? valid-field %))))))
