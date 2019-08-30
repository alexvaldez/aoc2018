(ns aoc.day07
  (:require [clojure.string :as s]))

(defn parse [input]
  (mapv (fn [line] (mapv s/trim (re-seq #" [A-Z] " line)))
        (s/split-lines (slurp input))))

(defn calc-graph
"creates a representation of the dependency graph
 heads - steps that can be executed now
 steps - a map from a step to its prerequisites
 prereqs - a map from a prerequisite and its dependent steps"
  [data]
  (reduce (fn [[heads steps prereqs] [prereq step]]
            [
             ; add prereq to heads if first time encountered
             ; and remove step from heads
             (disj (if-not (steps prereq)
                     (conj heads prereq) 
                     heads)
                   step)
             ; add step to steps and associate prereq to it
             (assoc steps
                    step
                    ((fnil conj #{}) (steps step) prereq))
             ; add prereq to prereqs and associate step to it
             (assoc prereqs
                    prereq
                    ((fnil conj #{}) (prereqs prereq) step))
            ])
          [(sorted-set) {} {}]
          data))

(defn get-next [[heads steps prereqs]]
  (first heads))

(defn start-step [[heads steps prereqs] step]
  [(disj heads step) steps prereqs])

(defn end-step [[heads steps prereqs] this-step]
  (reduce (fn [[h s p] next-step]
            (let [prereqs-remaining (disj (s next-step) this-step)]
              (if (empty? prereqs-remaining)
                [; next step is ready to execute
                 (conj h next-step) ; add to heads
                 (dissoc s next-step) ; remove from steps
                 p] ; no change to prereqs
                [; next step cannot begin
                 h ; no change to heads
                 (assoc s next-step prereqs-remaining) ; remove this-step
                 p]))) ; no change to prereqs
          [heads steps (dissoc prereqs this-step)]
          (prereqs this-step)))

(defn day07-1 [data]
  (loop [result []
         graph (calc-graph data)]
    (let [next-step (get-next graph)]
      (if (nil? next-step)
        (s/join result)
        (recur (conj result next-step)
               (end-step (start-step graph next-step) next-step))))))

(def workers 5)
(def delay 60)

(defn calc-end [step start-time]
  (+ start-time delay (- (int (first (char-array step))) (int \A)) 1))

(defn day07-2 [data]
  (loop [t 0
         wip {}
         graph (calc-graph data)]
    (let [next-step (get-next graph)]
      (if (and next-step (> workers (count wip)))
        (recur t
               (assoc wip next-step (calc-end next-step t))
               (start-step graph next-step))
        (if (empty? wip)
          t
          (let [[ended end-time] (apply min-key val wip)]
            (recur end-time
                   (dissoc wip ended)
                   (end-step graph ended))))))))








