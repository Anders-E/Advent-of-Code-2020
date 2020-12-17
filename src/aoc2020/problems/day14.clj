(ns aoc2020.problems.day14
  (:require [aoc2020.util :refer [get-input
                                  p]]
            [clojure.string :refer [split-lines]]))

(def input (get-input))

(defn parse-input [input]
  (map (comp (p drop 1)
             #(or (re-matches #"mem\[(\d+)\] = (\d+)" %)
                  (re-matches #"mask = (\w+)" %)))
       (split-lines input)))

(defn set-bit [num n x]
  (bit-xor num
           (bit-and (bit-xor (- x)
                             num)
                    (bit-shift-left 1 n))))

(defn char->int [c] (Character/getNumericValue c))

(defn mask-val [mask val]
  (reduce (fn [num [n x]] (set-bit num n x))
          val
          mask))

(defn parse-mask [mask]
  (->> (map-indexed (fn [i x] [(- 35 i) x]) mask)
       (remove #(= (second %) \X))
       (map (fn [[i x]] [i (char->int x)]))))

(defn run-instruction
  ([memory _ new-mask]
   [memory (parse-mask new-mask)])
  ([memory mask address value]
   (let [address (Integer. address)
         value   (Integer. value)]
     [(assoc memory address (mask-val mask value))
      mask])))

(defn run-program [program]
  (reduce (fn [[mem mask] cmd] (apply run-instruction mem mask cmd))
          [{} ""]
          program))

(defn star1
  ([] (star1 input))
  ([input]
   (apply + (vals (first (run-program (parse-input input)))))))

(defn star2
  ([] (star2 input))
  ([input]
   nil))
