(ns aoc.day09
  (:require [clojure.string :as s]))

(defn parse [input]
  (mapv #(Integer. %) (re-seq #"\d+" (slurp input))))

(defn add [arr i val]
  (into [] (concat (subvec arr 0 i) [val] (subvec arr i))))

(defn delete [arr i]
  (into [] (concat (subvec arr 0 i) (subvec arr (inc i)))))

(defn day09-1 [[nplayers nmarbles]]
  (def arr (java.util.ArrayList.))
  (.add arr 0 0)
  (let [[curr scores]
    (reduce (fn [[curr scores] n]
              ;(println (.toString arr))
              (if (= 0 (mod n 23))
                (let [marble (mod (- curr 7) (count arr))
                      score (+ n (.get arr marble))]
                  (if (= 0 (mod n 1000))
                    (do
                      (printf "n %d marble %d score %d" n (.get arr marble) score)
                      (println)))
                  (.remove #^java.util.ArrayList arr (int marble))
                  [(mod marble (dec (.size arr)))
                   (update-in scores [(mod n nplayers)] (fnil #(+ score %) 0))])
                (let [nxt (inc (mod (inc curr) (.size arr)))]
                  (.add #^java.util.ArrayList arr (int nxt) n)
                  [nxt
                   scores])))
            [0 {}] 
            (range 1 (inc nmarbles)))]
    (apply max (vals scores))))

(defn day09-2 [[nplayers nmarbles]]
  (def before (int-array (inc nmarbles)))
  (def after (int-array (inc nmarbles)))
  (let [[curr scores]
    (reduce (fn [[curr scores] n]
              ;(println (.toString arr))
              (if (= 0 (mod n 23))
                (let [marble (nth (iterate (partial aget before) curr) 7)
                      nxt (aget after marble)
                      prv (aget before marble)
                      score (+ marble n)]
                  (aset after prv nxt)
                  (aset before nxt prv)
                  (if (= 0 (mod n 1000))
                    (do
                      (printf "n %d marble %d score %d" n marble score)
                      (println)))
                  [nxt
                   (update-in scores [(mod n nplayers)] (fnil #(+ score %) 0))])
                (let [nxt (nth (iterate (partial aget after) curr) 2)
                      prv (aget before nxt)]
                  (aset after n nxt)
                  (aset before n prv)
                  (aset after prv n)
                  (aset before nxt n)
                  [n
                   scores])))
            [0 {}] 
            (range 1 (inc nmarbles)))]
    (apply max (vals scores))))
