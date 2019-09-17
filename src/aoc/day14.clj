(ns aoc.day14
  (:require [clojure.string :as str]))

(defn to-digits [n]
  (map #(-' (int %) (int \0)) (seq (str n))))

(defn day14-1 [n]
  (loop [v [3 7]
         elf1 0
         elf2 1]
    (let [score (+ (v elf1) (v elf2))
          newv (into v (to-digits score))
          newelf1 (mod (+ elf1 (v elf1) 1) (count newv))
          newelf2 (mod (+ elf2 (v elf2) 1) (count newv))]
      (if (>= (count newv) (+ n 10))
        (str/join (subvec newv n (+ n 10)))
        (recur newv newelf1 newelf2)))))

(defn day14-2 [s]
  (let [to-match (vec (map #(-' (int %) (int \0)) (seq s)))
        match-len (count s)]
    (loop [v [3 7]
           elf1 0
           elf2 1]
      (let [score (+ (v elf1) (v elf2))
            newv (into v (to-digits score))
            newelf1 (mod (+ elf1 (v elf1) 1) (count newv))
            newelf2 (mod (+ elf2 (v elf2) 1) (count newv))]
        (if (and 
              (>= (count newv) (count to-match))
              (= to-match (subvec newv (- (count newv) match-len))))
          (- (count newv) match-len)
          (if (and 
                (> score 9)
                (> (count newv) (count to-match))
                (= to-match (subvec newv (- (count newv) match-len 1) (dec (count newv)))))
            (- (count newv) match-len 1)
            (recur newv newelf1 newelf2)))))))



