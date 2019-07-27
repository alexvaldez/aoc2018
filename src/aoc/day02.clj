(ns aoc.day02
  (:require [clojure.string :as str]))

(defn parse [input]
  (mapv #(str/split % #"")
        (str/split-lines input)))

(defn count-letters [letters]
  (reduce (fn [m c]
            (update m c (fnil inc 0)))
          {}
          letters))

(defn day02-1 [data]
  (reduce (fn [counts letters]
            (let [[twos threes] counts
                  letter-counts (vals (count-letters letters))]
              [(if (some #{2} letter-counts) (+ 1 twos) twos)
               (if (some #{3} letter-counts) (+ 1 threes) threes)]))
          [0 0]
          data))
