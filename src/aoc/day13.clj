(ns aoc.day13
  (:require [clojure.string :as str]))

(defn cart-comparator [cart1 cart2]
  (let [c (compare (cart1 0) (cart2 0))]
    (if (not= c 0)
      c
      (compare (cart1 1) (cart2 1)))))

(defn parse [input]
   (let [lines (str/split-lines (slurp input))]
     (reduce (fn [[carts field] line]
               [(reduce (fn [carts i]
                          (let [c (first (subs line i))]
                            (if (#{\< \> \^ \v} c)
                              (conj carts [i (count field) c 0])
                              carts)))
                        carts
                        (range (count line)))
                (conj field
                      (-> line
                          (str/replace #"[<>]" "-") 
                          (str/replace #"[v^]" "|")
                          (vec)))])
             [(sorted-set-by cart-comparator) []]
             lines)))

(defn advance
  [[x y dir st]]
  (case dir
    \^ [x (dec y) dir st]
    \v [x (inc y) dir st]
    \< [(dec x) y dir st]
    \> [(inc x) y dir st]))

(def turn-table
  {\< {\/ \v \\ \^}
   \> {\/ \^ \\ \v} 
   \^ {\/ \> \\ \<} 
   \v {\/ \< \\ \>}}) 

(def intersection-table
  {\< {0 \v 1 \< 2 \^}
   \> {0 \^ 1 \> 2 \v}
   \^ {0 \< 1 \^ 2 \>}
   \v {0 \> 1 \v 2 \<}})

(defn check-turn [cart terrain]
  (let [[x y dir st] cart
        p (get-in terrain [y x])
        newdir (get-in turn-table [dir p])]
    (if newdir
      [x y newdir st]
      cart)))

(defn check-intersection [cart terrain]
  (let [[x y dir st] cart
        p (get-in terrain [y x])]
    (if (= p \+)
      [x y (get-in intersection-table [dir st]) (mod (inc st) 3)]
      cart)))

(defn dump [carts terrain]
  (doseq [line
          (reduce (fn [terrain [x y dir st]]
                   (assoc-in terrain [y x] dir))
                  terrain
                  carts)]
    (println (str/join line))))

(defn stop-on-collision [carts old new]
  (let [collision (carts new)]
    (if collision
      (reduced #{collision})
      (-> carts
          (disj old)
          (conj new)))))

(defn clear-collision [carts old new]
  (let [collision (carts new)]
    (if collision
      (-> carts
          (disj old)
          (disj collision))
      (-> carts
          (disj old)
          (conj new)))))

(defn simulate [carts terrain collision-check]
  (loop [carts carts]
    (let [newcarts
          (reduce (fn [newcarts cart]
                    (if (newcarts cart)
                      (let [newcart (-> cart
                                        (advance)
                                        (check-turn terrain)
                                        (check-intersection terrain))]
                        (collision-check newcarts cart newcart))
                      newcarts))
                  carts
                  carts)]
      (if (> (count newcarts) 1)
        (do
          (dump newcarts terrain)
          (if (= "" (read-line))
            (recur newcarts)))
        (let [answer (first newcarts)]
          (and answer (str/join "," (subvec answer 0 2))))))))

(defn day13-1 [[carts terrain]]
  (simulate carts terrain stop-on-collision))

(defn day13-2 [[carts terrain]]
  (simulate carts terrain clear-collision))
