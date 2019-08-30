(ns aoc.day02
  (:require [clojure.string :as str]))

(defn parse [input]
  (str/split-lines (slurp input)))

(defn count-letters [line]
  (reduce (fn [m c]
            (update m c (fnil inc 0)))
          {}
          (str/split line #"")))

(defn day02-1 [data]
  (apply *
  (reduce (fn [counts letters]
            (let [[twos threes] counts
                  letter-counts (vals (count-letters letters))]
              [(if (some #{2} letter-counts) (inc twos) twos)
               (if (some #{3} letter-counts) (inc threes) threes)]))
          [0 0]
          data)))

(defn submatches [data n]
  (reduce (fn [substrs s]
            (let [substr (str (subs s 0 n) (subs s (inc n)))]
              (if (substrs substr)
                (reduced substr)
                (conj substrs substr))))
          #{}
          data))

(defn day02-2 [data]
  (first (filter string?
                 (map #(submatches data %)
                      (range (count (first data)))))))

