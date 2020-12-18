(ns aoc2020.problems.day17
  (:require [aoc2020.util :refer [get-input
                                  index]]
            [clojure.string :refer [split-lines]]))

(def input (get-input))

(defn parse-input [input]
  (set (for [[y row] (index (split-lines input))
             [x cube] (index row)
             :when (= cube \#)]
         [x y 0])))

(defn neighbors
  ([x y z]
   (for [x' (range (dec x) (+ x 2))
         y' (range (dec y) (+ y 2))
         z' (range (dec z) (+ z 2))
         :when (not= [x y z] [x' y' z'])]
     [x' y' z']))
  ([x y z w]
   (for [x' (range (dec x) (+ x 2))
         y' (range (dec y) (+ y 2))
         z' (range (dec z) (+ z 2))
         w' (range (dec w) (+ w 2))
         :when (not= [x y z w] [x' y' z' w'])]
     [x' y' z' w'])))

(defn active-neighbor-counts [active-cubes]
  (->> (map #(apply neighbors %) active-cubes)
       (apply concat)
       (frequencies)))

(defn activate? [active-cubes cube active-neighbors]
  (let [active? (contains? active-cubes cube)]
    (or (and active?
             (<= 2 active-neighbors 3))
        (and (not active?)
             (= active-neighbors 3)))))

(defn update-grid [active-cubes]
  (->> (active-neighbor-counts active-cubes)
       (filter #(apply activate? active-cubes %))
       (map first)
       (set)))

(defn star1
  ([] (star1 input))
  ([input]
   (let [active-cubes (parse-input input)]
     (-> (update-grid active-cubes)
         (update-grid) (update-grid) (update-grid) (update-grid) (update-grid)
         (count)))))

(defn star2
  ([] (star2 input))
  ([input]
   (let [active-cubes (set (map #(conj % 0) (parse-input input)))]
     (-> (update-grid active-cubes)
         (update-grid) (update-grid) (update-grid) (update-grid) (update-grid)
         (count)))))
