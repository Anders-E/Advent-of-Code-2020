(ns aoc2020.test-stars
  (:require [clojure.test :refer [deftest
                                  is]]
            [aoc2020.util :refer [get-answer
                                  get-input]]))

(deftest day01
  (for [day (range 1 4)]
    (let [input  (str (get-input day))
          answer (get-answer day)]
      (is (= input
             answer)))))
