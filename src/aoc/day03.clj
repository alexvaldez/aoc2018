(ns aoc.day03
  (:require [clojure.string :as s]))

(defn parse [input]
  (mapv (fn [line] (mapv #(Integer. %) (re-seq #"\d+" line)))
        (s/split-lines input)))


