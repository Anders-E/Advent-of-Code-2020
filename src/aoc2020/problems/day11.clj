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

(defn free? [seat] (= seat \L))

(defn occupied? [seat] (= seat \#))

(defn floor? [seat] (= seat \.))

(defn get-bounds [plan]
  [(count (first plan)) (count plan)])

(defn out-of-bounds [plan [x y]]
  (let [[x-max y-max] (get-bounds plan)]
    (not (and (<= 0 x (dec x-max))
              (<= 0 y (dec y-max))))))

(defn adjacent-coords [x y]
  (remove-vals [x y]
               (for [x (range (dec x) (+ x 2))
                     y (range (dec y) (+ y 2))]
                 [x y])))

(defn adjacent-seats [plan x y]
  (->> (adjacent-coords x y)
       (remove (p out-of-bounds plan))
       (map (p getget plan))
       (remove-vals \.)))

(defn blocks-view [plan [x y]]
  (or (out-of-bounds plan [x y])
      (floor? (getget plan [x y]))))

(defn first-seat-in-view [plan coords]
  (let [[_ blocked] (split-with (p blocks-view plan) coords)]
    (first blocked)))


(defn all-dirs [plan x y]
  (let [[x-max y-max] (get-bounds plan)
        n             (map vector (repeat x) (range (inc y) y-max))
        e             (map vector (range (inc x) x-max) (repeat y))
        s             (map vector (repeat x) (range (dec y) -1 -1))
        w             (map vector (range (dec x) -1 -1) (repeat y))
        nw            (map vector (range (dec x) -1 -1) (range (inc y) y-max))
        sw            (map vector (range (dec x) -1 -1) (range (dec y) -1 -1))
        ne            (map vector (range (inc x) x-max) (range (inc y) y-max))
        se            (map vector (range (inc x) x-max) (range (dec y) -1 -1))]
    [n e s w nw sw ne se]))


(defn seats-in-view [plan x y]
  (->> (map (p first-seat-in-view plan) (all-dirs plan x y))
       (map (p getget plan))
       (remove-vals nil)))

(defn update-seat [plan x y find-seats-fn]
  (let [seat      (getget plan x y)
        occupieds (filter occupied? (find-seats-fn plan x y))
        tolerance ({adjacent-seats 4 seats-in-view 5} find-seats-fn)]
    (cond
      (and (free? seat)
           (empty? occupieds)) \#
      (and (occupied? seat)
           (>= (count occupieds) tolerance)) \L
      :else seat)))

;; TODO: Speed up; use one-dimensional vector and reduce
;; or some transient shit
(defn update-seats [plan find-seats-fn]
  (let [[x-max y-max] (get-bounds plan)]
    (vec (for [y (range y-max)]
           (vec (for [x (range x-max)]
                  (update-seat plan x y find-seats-fn)))))))

(defn stabilize [plan find-seats-fn]
  (let [new-plan (update-seats plan find-seats-fn)]
    (if (= plan new-plan)
      plan
      (stabilize new-plan find-seats-fn))))

(defn star1
  ([] (star1 input))
  ([input]
   (let [plan (parse-input input)]
     (filter-count occupied? (flatten (stabilize plan adjacent-seats))))))

(defn star2
  ([] (star2 input))
  ([input]
   (let [plan (parse-input input)]
     (filter-count occupied? (flatten (stabilize plan seats-in-view))))))
