(ns aoc2020.problems.day08
  (:require [aoc2020.util :refer [get-input]]
            [clojure.string :refer [split
                                    split-lines]]))

(def input (get-input))

(defn parse-input [input]
  (conj (->> (split-lines input)
             (map (comp (fn [[a b]] [(keyword a) (Integer. b)])
                        #(split % #" ")))
             (vec))
        [:end nil]))

(defmulti exec (fn [prog pc _ _] (first (get prog pc))))

(defmethod exec :acc
  [prog pc acc visited]
  (if (contains? visited pc)
    acc
    (let [arg (second (get prog pc))]
      (exec prog (inc pc) (+ acc arg) (conj visited pc)))))

(defmethod exec :jmp
  [prog pc acc visited]
  (if (contains? visited pc)
    acc
    (let [arg (second (get prog pc))]
      (exec prog (+ pc arg) acc (conj visited pc)))))

(defmethod exec :nop
  [prog pc acc visited]
  (if (contains? visited pc)
    acc
    (exec prog (inc pc) acc (conj visited pc))))

(defmethod exec :end
  [_ _ acc _]
  [:halted acc])

(defn switch-jmp-nop [[op arg]]
  (let [switch {:nop :jmp :jmp :nop}]
    [(get switch op op) arg]))

(defn switch-ops [prog]
  (remove #(= % prog)
          (distinct (map-indexed #(assoc prog %1 (switch-jmp-nop %2))
                                 prog))))

(defn diff-index [a b]
  (let [zip (map vector a b)]
    (- (count a)
       (count (drop-while (fn [[a b]] (= a b)) zip)))))

(defn star1
  ([] (star1 input))
  ([input]
   (exec (parse-input input) 0 0 #{})))

(defn star2
  ([] (star2 input))
  ([input]
   (first (remove #(= % false)
                  (let [prog (parse-input input)]
                    (for [mod-prog (switch-ops prog)]
                      (let [acc (exec mod-prog 0 0 #{})]
                        (if (coll? acc)
                          (second acc)
                          false))))))))
