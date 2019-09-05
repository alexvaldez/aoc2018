(ns aoc.day09
  (:require [clojure.string :as s]))

(defn parse [input]
  (mapv #(Integer. %) (re-seq #"\d+" (slurp input))))

(defn add [arr i val]
  (into [] (concat (subvec arr 0 i) [val] (subvec arr i))))

(defn delete [arr i]
  (into [] (concat (subvec arr 0 i) (subvec arr (inc i)))))

; TODO rewrite to use Java arrays for better performance?
(defn day09-1 [[nplayers nmarbles]]
  (let [[arr curr scores]
    (reduce (fn [[arr curr scores] n]
              (if (= 0 (mod n 23))
                (let [marble (mod (- curr 7) (count arr))
                      score (+ n (arr marble))]
                  [(delete arr marble)
                   (mod marble (dec (count arr)))
                   (update-in scores [(mod n nplayers)] (fnil #(+ score %) 0))])
                (let [nxt (inc (mod (inc curr) (count arr)))]
                  [(add arr nxt n)
                   nxt
                   scores])))
            [[0] 0 {}] 
            ;(drop 1 (range))))
            (range 1 (inc nmarbles)))]
    (apply max (vals scores))))

