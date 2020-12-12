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
  ([]
   (let [day (Integer. (second (re-find #"day(\d\d)" (str *ns*))))]
     (get-input day)))
  ([day]
   (slurp-resource (format "input/day%02d.txt" day))))

(def p
  "Alias for `partial`."
  partial)

(defn filter-count
  "Combined filter and count."
  [pred coll]
  (count (filter pred coll)))

(defn getget
  "The one true 'double get'"
  ([coll [x y]]
   (getget coll x y))
  ([coll x y]
   (get (get coll y) x)))

(defn map-keys
  [f m]
  (apply merge (for [[k v] m] {(f k) v})))

(defn map-values
  [f m]
  (apply merge (for [[k v] m] {k (f v)})))

(defn filter-vals
  [val coll]
  (filter (p = val) coll))

(defn remove-vals
  [val coll]
  (remove (p = val) coll))

(defn in?
  "Check if value is in collection."
  [x coll]
  (not (nil? (some #(= % x) coll))))

(defn fifo-queue
  "Wrapper for PersistentQueue."
  [& [coll]]
  (reduce conj clojure.lang.PersistentQueue/EMPTY coll))
