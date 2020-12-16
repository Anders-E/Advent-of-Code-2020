(ns aoc2020.problems.day12
  (:require [aoc2020.util :refer [get-input
                                  p]]
            [clojure.string :refer [split-lines]]))

(def input (get-input))

(defn parse-input [input]
  (map (comp (fn [[_ dir dist]] [(keyword dir) (Integer. dist)])
             (p re-matches #"(\w)(\d+)"))
       (split-lines input)))

(def directions {0 :E 90 :S 180 :W 270 :N})

(defmulti move (fn [[action _] _ _ _] action))

(defmethod move :N [[_ val] x y angle] [x (+ y val) angle])

(defmethod move :S [[_ val] x y angle] [x (- y val) angle])

(defmethod move :E [[_ val] x y angle] [(+ x val) y angle])

(defmethod move :W [[_ val] x y angle] [(- x val) y angle])

(defmethod move :L [[_ val] x y angle] [x y (mod (- angle val) 360)])

(defmethod move :R [[_ val] x y angle] [x y (mod (+ angle val) 360)])

(defmethod move :F [[_ val] x y angle]
  (let [dir (directions angle)]
    (move [dir val] x y angle)))

(defn navigate [instructions x y angle]
  (if (empty? instructions)
    [x y]
    (let [instruction (first instructions)
          [x y angle] (move instruction x y angle)]
      (navigate (drop 1 instructions) x y angle))))

(def sin {0 0 90 1 180 0 270 -1
          -90 -1 -180 0 -270 1})

(def cos {0 1 90 0 180 -1 270 0
          -90 0 -180 -1 -270 0})

(defn rotate [[x y] angle]
  [(+ (* x (cos angle))
      (* y (sin angle)))
   (- (* y (cos angle))
      (* x (sin angle)))])

(defmulti move-waypoint (fn [[action _] _ _] action))

(defmethod move-waypoint :N [[_ val] ship [x y]] [ship [x (+ y val)]])

(defmethod move-waypoint :S [[_ val] ship [x y]] [ship [x (- y val)]])

(defmethod move-waypoint :E [[_ val] ship [x y]] [ship [(+ x val) y]])

(defmethod move-waypoint :W [[_ val] ship [x y]] [ship [(- x val) y]])

(defmethod move-waypoint :L [[_ val] ship waypoint] [ship (rotate waypoint (- val))])

(defmethod move-waypoint :R [[_ val] ship waypoint] [ship (rotate waypoint val)])

(defmethod move-waypoint :F [[_ val] ship waypoint]
  [(->> (map (p * val) waypoint)
        (map + ship)
        (vec))
   waypoint])

(defn navigate-waypoint [instructions ship waypoint]
  (if (empty? instructions)
    ship
    (let [instruction     (first instructions)
          [ship waypoint] (move-waypoint instruction ship waypoint)]
      (navigate-waypoint (drop 1 instructions) ship waypoint))))

(defn manhattan-distance [ship] (apply + (map #(Math/abs %) ship)))

(defn star1
  ([] (star1 input))
  ([input]
   (let [instructions (parse-input input)]
     (manhattan-distance (navigate instructions 0 0 0)))))

(defn star2
  ([] (star2 input))
  ([input]
   (let [instructions (parse-input input)]
     (manhattan-distance (navigate-waypoint instructions [0 0] [10 1])))))
