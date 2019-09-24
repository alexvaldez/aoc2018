(ns aoc.day16
  (:require [clojure.string :as str]))

(defn to-digits [line]
  (mapv #(Integer. %) (re-seq #"\d+" line)))

(defn parse [input]
  (loop [lines (str/split-lines (slurp input))
         examples []]
    (if (str/starts-with? (first lines) "Before:")
      (recur (drop 4 lines)
             (conj examples [(to-digits (second lines))
                             (to-digits (first lines))
                             (to-digits (nth lines 2))]))
      [examples (mapv to-digits (filter #(not= % "") lines))])))

(defn addr
  "add register: stores into register C the result of adding register A and register B."
  [[opcode a b c] r]
  (assoc-in r [c] (+ (r a) (r b)))
  )

(defn addi
  "add immediate: stores into register C the result of adding register A and value B."
  [[opcode a b c] r]
  (assoc-in r [c] (+ (r a) b))
  )

(defn mulr
  "multiply register: stores into register C the result of multiplying register A and register B."
  [[opcode a b c] r]
  (assoc-in r [c] (* (r a) (r b)))
  )

(defn muli
  "multiply immediate: stores into register C the result of multiplying register A and value B."
  [[opcode a b c] r]
  (assoc-in r [c] (* (r a) b))
  )

(defn banr
  "bitwise AND register: stores into register C the result of the bitwise AND of register A and register B."
  [[opcode a b c] r]
  (assoc-in r [c] (bit-and (r a) (r b)))
  )

(defn bani
  "bitwise AND immediate: stores into register C the result of the bitwise AND of register A and value B."
  [[opcode a b c] r]
  (assoc-in r [c] (bit-and (r a) b))
  )

(defn borr
  "bitwise OR register: stores into register C the result of the bitwise OR of register A and register B."
  [[opcode a b c] r]
  (assoc-in r [c] (bit-or (r a) (r b)))
  )

(defn bori
  "bitwise OR immediate: stores into register C the result of the bitwise OR of register A and value B."
  [[opcode a b c] r]
  (assoc-in r [c] (bit-or (r a) b))
  )

(defn setr
  "set register: copies the contents of register A into register C. (Input B is ignored.)"
  [[opcode a b c] r]
  (assoc-in r [c] (r a))
  )

(defn seti
  "set immediate: stores value A into register C. (Input B is ignored.)"
  [[opcode a b c] r]
  (assoc-in r [c] a)
  )

(defn gtir
  "greater-than immediate/register: sets register C to 1 if value A is greater than register B. Otherwise, register C is set to 0."
  [[opcode a b c] r]
  (assoc-in r [c] (if (> a (r b)) 1 0))
  )

(defn gtri
  "greater-than register/immediate: sets register C to 1 if register A is greater than value B. Otherwise, register C is set to 0."
  [[opcode a b c] r]
  (assoc-in r [c] (if (> (r a) b) 1 0))
  )

(defn gtrr
  "greater-than register/register: sets register C to 1 if register A is greater than register B. Otherwise, register C is set to 0."
  [[opcode a b c] r]
  (assoc-in r [c] (if (> (r a) (r b)) 1 0))
  )

(defn eqir
  "equal immediate/register: sets register C to 1 if value A is equal to register B. Otherwise, register C is set to 0."
  [[opcode a b c] r]
  (assoc-in r [c] (if (= a (r b)) 1 0))
  )

(defn eqri
  "equal register/immediate: sets register C to 1 if register A is equal to value B. Otherwise, register C is set to 0."
  [[opcode a b c] r]
  (assoc-in r [c] (if (= (r a) b) 1 0))
  )

(defn eqrr
  "equal register/register: sets register C to 1 if register A is equal to register B. Otherwise, register C is set to 0."
  [[opcode a b c] r]
  (assoc-in r [c] (if (= (r a) (r b)) 1 0))
  )

(defn run-tests [examples]
  (reduce (fn [test-results [op [instr before after]]]
            (let [[opcode a b c] instr]
              (if (= (op instr before) after) ; pass
                (if (get-in test-results [op :fail opcode]) ; already failed
                 test-results ; do nothing
                  (update-in test-results [op :pass] (fnil conj #{}) opcode)) ; add to pass
                (if (get-in test-results [op :pass opcode]) ; was passed
                  (update-in test-results [op :pass] disj opcode) ; remove from pass
                 (update-in test-results [op :fail] (fnil conj #{}) opcode))))) ; add to fail
          {}
          (for [op [addr addi mulr muli banr bani borr bori setr seti gtir gtri gtrr eqir eqri eqrr]
                ex examples]
            [op ex])))

(defn count-matches [examples]
  (map (fn [[instr before after]]
         (count (filter (fn [op]
                          (= (op instr before) after))
                        [addr addi mulr muli banr bani borr bori setr seti gtir gtri gtrr eqir eqri eqrr])))
       examples))

(defn day16-1 [[examples prog]]
  (count (filter #(>= % 3) (count-matches examples))))

(defn determine-isa [test-results]
  (loop [isa {}]
    (let [to-add (reduce (fn [to-add test-result]
                           (let [passed (apply disj ((val test-result) :pass) (keys isa))]
                             (if (= (count passed) 1)
                               (assoc-in to-add [(first passed)] (key test-result))
                               to-add)))
                         {}
                         test-results)]
      (if (not (empty? to-add))
        (recur (merge isa to-add))
        isa))))

(defn run-prog [isa prog]
  (reduce (fn [r instr]
            (let [[opcode a b c] instr]
              ((isa opcode) instr r)))
          [0 0 0 0]
          prog))

(defn day16-2 [[examples prog]]
  (let [test-results (run-tests examples)
        isa (determine-isa test-results)]
    (run-prog isa prog)))

