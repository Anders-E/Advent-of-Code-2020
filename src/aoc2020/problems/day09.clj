(ns aoc2020.problems.day09
  (:require [aoc2020.util :refer [fifo-queue
                                  get-input
                                  in?
                                  remove-vals]]
            [clojure.string :refer [split-lines]]))

(def input (get-input))

(defn parse-input [input] (map #(Integer. %) (split-lines input)))

(defn pop-and-queue [queue x]
  (conj (pop queue) x))

(defn first-faulty [nums prevs sums]
  (let [current (first nums)]
    (if (in? current (flatten sums))
      (first-faulty (drop 1 nums)
                    (pop-and-queue prevs
                                   current)
                    (pop-and-queue sums
                                   (map #(+ current %)
                                        (pop prevs))))
      current)))

(defn star1
  ([] (star1 input))
  ([input]
   (let [nums     (parse-input input)
         prevs    (fifo-queue (take 25 nums))
         preamble (fifo-queue (for [a prevs]
                                (for [b (remove-vals a prevs)]
                                  (+ a b))))]
     (first-faulty (drop 25 nums) prevs preamble))))

(defn star2
  ([] (star2 input))
  ([input]
   nil))
