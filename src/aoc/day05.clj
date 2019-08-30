(ns aoc.day05
  (:require [clojure.string :as s]))

(defn parse [input]
  (filter #(Character/isLetter %) (seq (slurp input))))

(def upper-case? #(and % (Character/isUpperCase %)))

(def to-upper #(and % (s/upper-case %)))

(defn depolarize [data]
  (reduce (fn [v c]
            (let [prev (peek v)]
              (if (and (not= (upper-case? prev) (upper-case? c))
                       (= (to-upper prev) (to-upper c)))
                (pop v)
                (conj v c))))
          []
          data))

(defn filter-letter [chars c]
  (filter #(not= (to-upper c) (to-upper %)) chars))

(defn day05-1 [data]
  (count (depolarize data)))

(defn day05-2 [data]
  (let [depolarized (depolarize data)]
    (apply min
           (map (fn [c]
                  (->> c
                       (char)
                       (filter-letter depolarized)
                       (depolarize)
                       (count)))
                (range (int \a) (inc (int \z)))))))
