(ns aoc.day04
  (:require [clojure.string :as s]))

(defn parse [input]
  (mapv (fn [line] (re-seq #"\d+|falls asleep|wakes up" line))
        (sort (s/split-lines (slurp input)))))

(defn make-table [data]
  (first (reduce
           (fn [[m guard slept] [yr mo dt hr mn evt]]
             (let [minute (Integer. mn)]
               (case evt
                 "falls asleep" [m guard minute]
                 "wakes up" [(update-in m [guard] (fnil conj []) [slept minute]) guard nil]
                 [m (Integer. evt) nil])))
           [{} nil nil]
           data)))

(defn total-time [entry]
  (reduce (fn [n [slept woke]]
            (+ n (- woke slept)))
          0
          (val entry)))

(defn max-minute [ranges]
  (apply max-key val
           (reduce (fn [m [slept woke]]
                     (reduce (fn [m minute]
                               (update-in m [minute] (fnil inc 0)))
                             m
                             (range slept woke)))
                     {}
                     ranges)))

(defn max-guard [table]
  (apply max-key total-time table))

(defn day04-1 [data]
  (let [guard (max-guard (make-table data))]
    (* (key guard) (key (max-minute (val guard))))))

(defn day04-2 [data]
  (->> data
       (make-table)
       (reduce (fn [m [k v]]
                 (assoc m k (max-minute v)))
               {})
       (apply max-key (comp val val))
       (#(* (key %) (key (val %))))))



