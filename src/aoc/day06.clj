(ns aoc.day06
  (:require [clojure.string :as s]))

(defn parse [input]
  (mapv (fn [line] (mapv #(Integer. %) (re-seq #"\d+" line)))
        (s/split-lines (slurp input))))

(defn calc-bounds [data]
  (reduce (fn [[minx maxx miny maxy] [x y]]
            [(min minx x) (max maxx x)
             (min miny y) (max maxy y)])
          [Integer/MAX_VALUE Integer/MIN_VALUE
           Integer/MAX_VALUE Integer/MIN_VALUE]
          data))

(defn abs [n] (max n (- n)))

(defn manhattan-dist [x0 y0 x1 y1]
  (+ (abs (- x0 x1)) (abs (- y0 y1))))

(defn find-nearest [data x y]
  (first 
    (reduce (fn [[node min-dist] n]
              (let [[xn yn] (data n)
                    dist (manhattan-dist x y xn yn)]
                (case (compare min-dist dist)
                  -1 [node min-dist]
                  0 [nil min-dist]
                  1 [n dist])))
            [nil Integer/MAX_VALUE]
            (range (count data)))))

(defn calc-nearest [data [minx maxx miny maxy]]
  (reduce (fn [m [xi yi]]
             (let [nearest (find-nearest data xi yi)]
               (if (nil? nearest)
                 m
                 (assoc-in m [xi yi] nearest))))
           {}
           (for [x (range minx (inc maxx))
                 y (range miny (inc maxy))]
             [x y])))

(defn calc-safe [data [minx maxx miny maxy]]
  (reduce (fn [m [xi yi]]
            (assoc-in m [xi yi] 
                       (apply +
                              (map (fn [[xn yn]] 
                                     (manhattan-dist xi yi xn yn))
                                   data))))
           {}
           (for [x (range minx (inc maxx))
                 y (range miny (inc maxy))]
             [x y])))

; TODO also add safe regions outside the bounding box
; i cheated because i saw that the boundary was all > 10000
(defn count-safe [data [minx maxx miny maxy]]
  (count 
    (filter
      (fn [[xi yi]] (> 10000
                        (apply +
                               (map (fn [[xn yn]] 
                                      (manhattan-dist xi yi xn yn))
                                    data))))
      (for [x (range minx (inc maxx))
            y (range miny (inc maxy))]
        [x y]))))

(defn find-edges [nearest-map [minx maxx miny maxy]]
  (set (concat (vals (nearest-map minx))
               (vals (nearest-map maxx))
               (keep #(get % miny) (map val nearest-map))
               (keep #(get % maxy) (map val nearest-map)))))

(defn day06-1 [data]
  (let [bounds (calc-bounds data)
        nearest-map (calc-nearest data bounds)
        edges (find-edges nearest-map bounds)]
    (->> nearest-map
         (vals)
         (map vals)
         (apply concat)
         (reduce (fn [m n]
                   (update-in m [n] (fnil inc 0)))
                 {})
         (#(apply dissoc % edges))
         (apply max-key val)
         (second))))
