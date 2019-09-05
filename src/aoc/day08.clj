(ns aoc.day08
  (:require [clojure.string :as s]))

(defn parse [input]
  (mapv #(Integer. %) (re-seq #"\d+" (slurp input))))

(defn parse-node [numbers]
  (let [nchildren (numbers 0)
        nmeta (numbers 1)
        [children meta-numbers]
          (reduce (fn [[siblings child-numbers] i]
                    (let [[node next-child-numbers] (parse-node child-numbers)]
                      [(conj siblings node)
                       next-child-numbers]))
                  [[] (subvec numbers 2)]
                  (range nchildren))]
    [{:children children
      :meta (subvec meta-numbers 0 nmeta)}
     (subvec meta-numbers nmeta)]))

(defn add-meta [node]
  (+ (apply + (node :meta))
     (reduce (fn [n child]
               (+ n (add-meta child)))
             0
             (node :children))))

(defn calc-value [node]
  (let [children (node :children)
        meta (node :meta)]
    (if (empty? children)
      (apply + meta)
      (reduce (fn [n i]
                (+ n
                   (if (and (> i 0) (<= i (count children)))
                     (calc-value (children (dec i)))
                     0)))
              0
              meta))))

(defn day08-1 [numbers]
  (let [node (first (parse-node numbers))]
    (add-meta node)))

(defn day08-2 [numbers]
  (let [node (first (parse-node numbers))]
    (calc-value node)))
