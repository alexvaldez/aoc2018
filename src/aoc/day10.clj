(ns aoc.day10
  (:require [clojure.string :as s]))

(defn parse [input]
  (mapv (fn [line] (mapv #(Integer. %) (re-seq #"[\-0-9]+" line)))
        (s/split-lines (slurp input))))

(defn move [data n]
  (mapv (fn [[x y dx dy]]
          [(+ x (* dx n)) (+ y (* dy n)) dx dy])
        data))

(defn calc-closest[data]
  (let [[sumxy sumdxdy]
        (reduce (fn [[x+y dx+dy] [x y dx dy]]
                  [(+ x+y (Math/abs x) (Math/abs y))
                   (+ dx+dy (Math/abs dx) (Math/abs dy))])
                [0 0]
                data)]
        (quot sumxy sumdxdy)))

(defn bounds [data]
  (reduce (fn [[minx miny maxx maxy] [x y dx dy]]
            [(min minx x)
             (min miny y)
             (max maxx x)
             (max maxy y)])
          [Integer/MAX_VALUE Integer/MAX_VALUE 
           Integer/MIN_VALUE Integer/MIN_VALUE]
          data))

(defn plot [data]
  (let [[minx miny maxx maxy] (bounds data)
        len (inc (- maxx minx))]
    (s/join "\n"
            (map (fn [yn]
                   (s/join
                     (reduce (fn [line [x y dx dy]]
                               (assoc-in line [(- x minx)] "*"))
                             (vec (repeat len "."))
                             (filter (fn [[x y dx dy]] (= yn y)) data))))
                 (range miny (inc maxy))))))

(defn day10 [data]
  (loop [n (calc-closest data)
         points (move data n)]
    (printf "After %d seconds\n" n)
    (println (plot points))
    (let [entry (read-line)]
      (if (empty? entry)
        (recur (inc n) (move points 1))
        (if (not= "q" entry)
          (recur (+ n (Integer. entry)) (move points (Integer. entry))))))))
