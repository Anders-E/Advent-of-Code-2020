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
  (slurp-resource (format "answers/day%02d/%c.txt"
                          day
                          ({1 \a 2 \b} star))))

(defn get-input
  "Get input for specified day"
  [day]
  (slurp-resource (format "input/day%02d.txt" day)))

(def p
  "Alias for `partial`."
  partial)

(defn filter-count
  "Combined filter and count."
  [pred coll]
  (count (filter pred coll)))

(defn map-keys
  [f m]
  (apply merge (for [[k v] m] {(f k) v})))

(defn map-values
  [f m]
  (apply merge (for [[k v] m] {k (f v)})))

(defn in?
  "Check if value is in collection."
  [x coll]
  (not (nil? (some #(= % x) coll))))
