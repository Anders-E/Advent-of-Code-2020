(ns aoc2020.problems.day09
  (:require [aoc2020.util :refer [fifo-queue
                                  get-input
                                  in?
                                  pop-and-queue
                                  remove-vals]]
            [clojure.string :refer [split-lines]]))

(def input (get-input))

(defn parse-input [input] (map #(Integer. %) (split-lines input)))

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

(def star1-answer 22406676)

(defn contiguous_set [nums queue sum]
  (if (= sum star1-answer)
    queue
    (if (< sum star1-answer)
      (contiguous_set (rest nums)
                      (conj queue (first nums))
                      (+ sum (first nums)))
      (contiguous_set nums
                      (pop queue)
                      (- sum (first queue))))))

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
   (let [set (contiguous_set (parse-input input)
                             (fifo-queue)
                             0)]
     (+ (apply min set)
        (apply max set)))))
