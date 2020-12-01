(ns aoc2020.util
  (:require [clojure.java.io :as io]))

(defn slurp-input
  "Get input for specified day"
  [day]
  (-> (format "input/day_%02d.txt" day)
      (io/resource)
      (io/file)
      (slurp)))
