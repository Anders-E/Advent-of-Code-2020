(ns aoc2020.problems.day11
  (:require [aoc2020.util :refer [filter-count
                                  getget
                                  get-input
                                  p
                                  remove-vals]]
            [clojure.string :refer [split-lines]]))

(def input (get-input))

(defn parse-input [input]
  (->> (split-lines input)
       (map vec)
       (vec)))

(defn adjacent-coords [x y]
  (remove-vals [x y]
               (for [x (range (dec x) (+ x 2))
                     y (range (dec y) (+ y 2))]
                 [x y])))

(defn out-of-bounds [plan [x y]]
  (let [x-max (dec (count (first plan)))
        y-max (dec (count plan))]
    (not (and (<= 0 x x-max)
              (<= 0 y y-max)))))

(defn adjacent-seats [plan x y]
  (->> (adjacent-coords x y)
       (remove (p out-of-bounds plan))
       (map (p getget plan))
       (remove-vals \.)))

(defn free? [seat] (= seat \L))

(defn occupied? [seat] (= seat \#))

(defn update-seat [plan x y]
  (let [seat       (getget plan x y)
        occuppieds (filter occupied? (adjacent-seats plan x y))]
    (cond
      (and (free? seat)
           (empty? occuppieds)) \#
      (and (occupied? seat)
           (>= (count occuppieds) 4)) \L
      :else seat)))

;; TODO: Speed up; use one-dimensional vector and reduce
;; or some transient shit
(defn update-seats [plan]
  (let [x-max (count (first plan))
        y-max  (count plan)]
    (vec (for [y (range y-max)]
           (vec (for [x (range x-max)]
                  (update-seat plan x y)))))))

(defn stabilize [plan]
  (let [new-plan (update-seats plan)]
    (if (= plan new-plan)
      plan
      (stabilize new-plan))))

(defn star1
  ([] (star1 input))
  ([input]
   (let [plan (parse-input input)]
     (filter-count occupied? (flatten (stabilize plan))))))

(defn star2
  ([] (star2 input))
  ([input]
   nil))
