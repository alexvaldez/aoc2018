(ns aoc.day01
  (:require [clojure.string :as str]))

(defn parse [s]
  (mapv #(Long/parseLong %)
        (str/split-lines (slurp s))))

(defn day01-1 [input]
  (apply + input))

(defn day01-2 [input]
  (reduce
          (fn [seen x]
            (if (seen x)
              (reduced x)
              (conj seen x)))
          #{}
          (reductions + (cycle input))))
