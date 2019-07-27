(ns aoc.day01
  (:require [clojure.string :as str]))

(defn parse [s]
  (mapv #(Long/parseLong %)
        (str/split-lines (slurp s))))

(defn day01-1 [input]
  (apply + input))

(defn day01-2 [input]
  (loop [seen #{}
         data (cycle input)
         sum 0]
    (let [newsum (+ sum (first data))]
      (if (contains? seen newsum)
        newsum
        (recur (conj seen newsum)
               (rest data)
               newsum)))))


