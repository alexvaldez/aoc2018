(ns aoc.day12
  (:require [clojure.string :as str]))

(defn parse [input]
  (let [lines (str/split-lines (slurp input))
        init-state (re-find #"[#.]+"(first lines))]
    [(apply sorted-set
            (filter
              (fn [i] (= (nth init-state i) \#))
              (range (count init-state))))
     (into {} (mapv (fn [line] (vec (re-seq #"[#.]+" line))) (subvec lines 2)))]))

(def final-gen 50000000000)
;(def final-gen 200)

(defn encode-pots
  ([pots]
   (encode-pots pots (range (first pots) (inc (last pots)))))
  ([pots seqn]
   (apply str (mapv #(if (pots %) "#" ".") seqn))))


(defn calc-gen [pots notes]
  (reduce (fn [next-pots i]
            (let [plants (encode-pots pots (range i (+ i 5)))]
              (if (= (notes plants) "#")
                (conj next-pots (+ i 2))
                next-pots)))
          (sorted-set)
          (range (- (first pots) 4) (+ (last pots) 4))))

(defn extrapolate [history seen-1st seen-next]
  (let [seen-1st-at (seen-1st :at)
        seen-1st-start (first (seen-1st :pots))
        seen-next-at (seen-next :at)
        seen-next-start (first (seen-next :pots))
        hist-map (reduce (fn [m hist-entry]
                           (conj m [(hist-entry :at) (hist-entry :pots)]))
                         {}
                         (vals history))
        ncycles (quot (- final-gen seen-1st-at) (- seen-next-at seen-1st-at))
        extra-steps (mod (- final-gen seen-1st-at) (- seen-next-at seen-1st-at))
        final-pots (hist-map (+ seen-1st-at extra-steps))
        offset (+ (* (- seen-next-start seen-1st-start) ncycles) (- (first final-pots) seen-1st-start))]
    (into (sorted-set) (map (partial + offset) final-pots))))

(defn simulate [init-pots notes ngen]
  (let [[pots _] (reduce (fn [[pots history] i]
                      (let [next-pots (calc-gen pots notes)
                            hist-key (encode-pots next-pots)
                            hist-entry {:at (inc i) :pots next-pots}
                            seen-1st (history hist-key)]
                        (if seen-1st
                          (reduced [(extrapolate history seen-1st hist-entry) history])
                          [next-pots (assoc-in history [hist-key] hist-entry)])))
                    [init-pots (sorted-map)]
                    (range ngen))]
    (apply + pots)))
  
(defn day12-1 [[init-pots notes]]
  (simulate init-pots notes 20))
  
(defn day12-2 [[init-pots notes]]
  (simulate init-pots notes final-gen))

