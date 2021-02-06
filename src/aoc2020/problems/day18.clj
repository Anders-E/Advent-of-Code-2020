(ns aoc2020.problems.day18
  (:require [aoc2020.util :refer [get-input
                                  remove-vals]]
            [clojure.string :refer [split-lines]]))

(def input (get-input))

;; Grammar
;;
;; EXPR    -> OPERAND (OP EXPR)*
;; OPERAND -> NUM | LPAREN EXPR RPAREN

;; Tokenization

(defn char->value [char]
  ({\1 1 \2 2 \3 3 \4 4 \5 5
    \6 6 \7 7 \8 8 \9 9
    \( \) \) \(
    \+ + \* *} char))

(defn tokenize [line]
  (->> (apply list line)
       (remove-vals \space)
       (map char->value)
       (reverse)))

(defn parse-input [input] (map tokenize (split-lines input)))

;; Create AST

(defn node [left op right] {:left left :op op :right right})

(defn dequeue! [atom]
  (let [x (first @atom)]
    (swap! atom rest)
    x))

(declare expr!)

(defn operand! [tokens]
  (if (number? (first @tokens))
    (dequeue! tokens)
    (do (dequeue! tokens)
        (expr! tokens))))

(defn expr! [tokens]
  (let [left (operand! tokens)
        op   (dequeue! tokens)]
    (if (fn? op)
      (node left op (expr! tokens))
      left)))

;; Evaluate AST

(defn eval-ast [ast]
  (if (number? ast)
   ast 
   (let [{:keys [left op right]} ast
         left  (eval-ast left)
         right (eval-ast right)]
     (op left right))))

(defn star1
  ([] (star1 input))
  ([input]
   (->> (parse-input input)
        (map atom)
        (map expr!)
        (map eval-ast)
        (apply +))))

(defn star2
  ([] (star2 input))
  ([input]
   nil))
