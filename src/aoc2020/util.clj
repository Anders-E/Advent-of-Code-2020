(ns aoc2020.util
  (:require [clojure.java.io :as io]))

(defn slurp-resource
  [path]
  (-> path
      (io/resource)
      (io/file)
      (slurp)))

(defn get-answer
  "Get correct answer for specified day if it exists."
  [day star]
  (slurp-resource (format "answers/day_%02d/%c.txt"
                          day
                          ({1 \a 2 \b} star))))

(defn get-input
  "Get input for specified day"
  [day]
  (slurp-resource (format "input/day_%02d.txt" day)))
