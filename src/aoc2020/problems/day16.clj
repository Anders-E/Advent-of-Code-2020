(ns aoc2020.problems.day16
  (:require [aoc2020.util :refer [get-input
                                  index
                                  p
                                  remove-vals
                                  transpose]]
            [clojure.set :refer [difference]]
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

(defn filter-invalid-fields [rules values]
  (filter (p invalid-all-rules? rules) values))

(defn contains-no-invalid-fields [rules values]
  (not-any? (p invalid-all-rules? rules) values))

(defn valid-rule-for-field [values rule]
  (every? #(rule %) values))

(defn valid-rules-for-field [rules values]
  (->> (map (p valid-rule-for-field values) rules)
       (map-indexed (fn [i valid] (if valid i false)))
       (remove-vals false)
       (set)))

(defn deduce-fields [valid-rules]
  (let [indiced-rules      (index valid-rules)
        rules-by-ambiguity (sort-by second #(< (count %1) (count %2)) indiced-rules)
        unambiguous-rules  (cons (first rules-by-ambiguity)
                                 (map (fn [[i v] [_ w]]
                                        [i (difference v w)])
                                      (drop 1 rules-by-ambiguity)
                                      rules-by-ambiguity))
        rule-to-field      (into {}
                                 (map (fn [[i v]]
                                        [(first v) i]))
                                 unambiguous-rules)]
    rule-to-field))

(defn star1
  ([] (star1 input))
  ([input]
   (let [[rules _ tickets] (parse-input input)]
     (->> (map (p filter-invalid-fields rules) tickets)
          (flatten)
          (apply +)))))

(defn star2
  ([] (star2 input))
  ([input]
   (let [[rules my-ticket tickets] (parse-input input)
         tickets           (filter (p contains-no-invalid-fields rules) tickets)
         fields            (transpose tickets)
         departure-rules   (range 0 6)]
     (as-> (map (p valid-rules-for-field rules) fields) $
       (deduce-fields $)
       (map $ departure-rules)
       (map (p nth my-ticket) $)
       (apply * $)))))
