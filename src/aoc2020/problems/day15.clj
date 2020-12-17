(ns aoc2020.problems.day15
  (:require [aoc2020.util :refer [fifo-queue
                                  get-input
                                  pop-and-queue]]
            [clojure.string :refer [split]]))

(def input (get-input))

(defn parse-input [input] (map #(Integer. %) (split input #",")))

(defn starting-nums->history [nums]
  (into {} (map-indexed (fn [i num]
                          [num (fifo-queue [(inc i)])])
                        nums)))

;; TODO: Tidy up
(defn save [history n turn]
  (if (contains? history n)
    (let [turns (history n)]
      (if (= (count turns) 1)
        (update history n #(conj % turn))
        (update history n #(pop-and-queue % turn))))
    (assoc history n (fifo-queue [turn]))))

(defn speak [history prev turn]
  (let [prev-turns (get history prev (fifo-queue))
        n          (if (= (count prev-turns) 2)
                     (apply - (reverse prev-turns))
                     0)]
    (lazy-seq (cons n
                    (speak (save history n turn)
                           n
                           (inc turn))))))

(defn star1
  ([] (star1 input))
  ([input]
   (let [nums    (parse-input input)
         history (starting-nums->history nums)
         prev    (last nums)
         turn    (inc (count nums))]
     (nth (concat nums
                  (speak history prev turn))
          2019))))

(defn star2
  ([] (star2 input))
  ([input]
   nil))
