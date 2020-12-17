(ns aoc2020.problems.day13
  (:require [aoc2020.util :refer [get-input
                                  p
                                  remove-vals]]
            [clojure.string :refer [split
                                    split-lines]]))

(def input (get-input))

(defn parse-input [input] 
  (let [[departure buses] (split-lines input)
        departure         (Integer. departure)
        buses             (->> (split buses #",")
                               (map #(if (= % "x") \x (Integer. %))))]
    [departure buses]))

(defn first-bus [timestamp buses]
  (loop [ts timestamp]
    (let [id (first (filter #(zero? (mod ts %)) buses))]
      (if id
        [ts id]
        (recur (inc ts))))))

(defn gcd [a b]
  (if (zero? a)
    [b 0 1]
    (let [[g x y] (gcd (mod b a) a)]
      [g, (- y (* (Math/floorDiv b a) x)), x])))

(defn inverse [a n]
  (let [[_ x _] (gcd a n)]
    (mod x n)))

(defn bus-moduli [buses]
  (->> (map-indexed (fn [i x] [x i]) buses)
       (remove #(= (first %) \x))
       (map (fn [[bus i]] [bus (mod (- bus i) bus)]))))  ;; BUGGGGG

(defn inner [buses [m a]]
  (let [b  (apply * (remove-vals m buses))
        b' (inverse b m)]
    (* a b b')))

(defn star1
  ([] (star1 input))
  ([input]
   (let [[departure buses] (parse-input input)
         buses             (remove-vals \x buses)
         [wait bus]        (first-bus departure buses)]
     (* (- wait departure)
        bus))))

(defn star2
  ([] (star2 input))
  ([input]
   (let [[_ buses]   (parse-input input)
         bus-moduli  (bus-moduli buses)
         buses       (remove-vals \x buses)
         M           (apply * buses)]
     
    ;;  (mod (apply +
    ;;              (map (p inner buses)
    ;;                   bus-indices))
    ;;       M)
     
     bus-moduli
     
     )))
