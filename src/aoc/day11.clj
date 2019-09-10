(ns aoc.day11
  (:require [clojure.string :as s]))

(defn power-level [serial x y]
  (let [rackId (+ x 10)]
    (-> rackId
        (* y)
        (+ serial)
        (* rackId)
        (quot 100)
        (mod 10)
        (- 5))))

(defn calc-grid [serial]
  (reduce (fn [grid [x y]]
            (assoc-in grid [x y] (power-level serial x y)))
          (sorted-map)
          (for [x (range 1 301)
                y (range 1 301)]
            [x y])))

(defn calc-total
  ([grid x y]
   (calc-total grid x y 3))
  ([grid x y sz]
   (reduce (fn [sum [xn yn]]
             (+ sum (get-in grid [xn yn])))
           0
           (for [xn (range x (min (+ x sz) 301))
                 yn (range y (min (+ y sz) 301))]
             [xn yn]))))



(defn day11-1 [serial]
  (let [grid (calc-grid serial)]
    (apply max-key (fn [[x y]]
                     (calc-total grid x y))
           (for [x (range 1 301) y (range 1 301)] [x y]))))

; brute force - not efficient enough
;(defn day11-2 [serial]
;  (let [grid (calc-grid serial)]
;    (apply max-key (fn [[x y sz]]
;                     (calc-total grid x y sz))
;           (for [x (range 1 301)
;                 y (range 1 301)
;                 sz (range 300)]
;             [x y sz]))))

; TODO still not fast enough - convert to arrays
(defn calc-totals [grid]
  (reduce (fn [totals [sz x y]]
             (if (and (= x 1) (= y 1)) (println sz))
             (assoc-in totals [sz x y]
                       (+ (get-in totals [(dec sz) x y])
                          (apply + (for [i (range 0 (dec sz))]
                                     (get-in grid [(+ x (dec sz)) (+ y i)]))) 
                          (apply + (for [i (range 0 sz)]
                                     (get-in grid [(+ x i) (+ y (dec sz))])))))) 
           {1 grid}
           (for [sz (range 2 301) x (range 1 (- 301 sz)) y (range 1 (- 301 sz))] [sz x y])))
  
(defn day11-2 [serial]
  (let [grid (calc-grid serial)
        totals (calc-totals grid)]
    (apply max-key (fn [[sz x y]] (get-in totals [sz x y]))
           (for [sz (range 1 301) x (range 1 (- 301 sz)) y (range 1 (- 301 sz))] [sz x y]))))
