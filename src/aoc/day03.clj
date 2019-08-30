(ns aoc.day03
  (:require [clojure.string :as s]))

(defn parse [input]
  (mapv (fn [line] (mapv #(Integer. %) (re-seq #"\d+" line)))
        (s/split-lines (slurp input))))

(defn make-grid [data]
  (reduce (fn [m [n x y w h]]
            (reduce (fn [m x1]
                      (reduce (fn [m y1]
                                (update-in m [x1 y1] (fnil inc 0)))
                              m
                              (range y (+ y h))))
                    m
                    (range x (+ x w))))
          {}
          data))

(defn count2s [row]
  (->> row 
       (filter #(>= % 2))
       (count)))

(defn day03-1 [data]
  (let [grid (make-grid data)]
    (apply +
           (map count2s (map vals (vals grid))))))

(defn day03-2 [data]
  (let [grid (make-grid data)]
    (filter (fn [[n x y w h]]
              (empty? 
                (filter #(not= % 1)
                        (for [x1 (range x (+ x w)) y1 (range y (+ y h))]
                          (get-in grid [x1 y1])))))
            data)))



