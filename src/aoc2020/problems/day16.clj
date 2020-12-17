(ns aoc2020.problems.day16
  (:require [aoc2020.util :refer [get-input
                                  p]]
            [clojure.string :refer [split
                                    split-lines]]))

(def input (get-input))

(def rule-re #"(.+): (\d+)-(\d+) or (\d+)-(\d+)")

(def your-ticket-re #"your ticket:\n(.*)\n")

(def nearby-tickets-re #"(?s)nearby tickets:\n(.*)")

(defn rule [a b c d]
  (fn [x] (or (<= a x b)
              (<= c x d))))

(defn parse-rules [input]
  (for [[_ _ a b c d] (re-seq rule-re input)]
    (apply rule
           (map #(Integer. %)
                [a b c d]))))

(defn parse-your-ticket [input]
  (map #(Integer. %) (-> (re-find your-ticket-re input)
                         (second)
                         (split #","))))

(defn parse-nearby-tickets [input]
  (->> (re-find nearby-tickets-re input)
       (second)
       (split-lines)
       (map (comp (p map #(Integer. %))
                  #(split % #",")))))

(defn parse-input [input]
  (let [rules          (parse-rules input)
        your-ticket    (parse-your-ticket input)
        nearby-tickets (parse-nearby-tickets input)]
    [rules your-ticket nearby-tickets]))

(defn invalid-all-rules? [rules value]
  (every? #(not (% value)) rules))

(defn filter-any-invalid-values [rules values]
  (filter (p invalid-all-rules? rules) values))

(defn star1
  ([] (star1 input))
  ([input]
   (let [[rules _ tickets] (parse-input input)]
     (->> (map (p filter-any-invalid-values rules) tickets)
          (flatten)
          (apply +)))))

(defn star2
  ([] (star2 input))
  ([input]
   nil))
