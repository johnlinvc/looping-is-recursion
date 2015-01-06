(ns looping-is-recursion)

(defn power [base exp]
  (let [power-helper (fn [base exp acc]
                       (if (zero? exp)
                         acc
                         (recur base (dec exp) (* acc base) )))]
    (power-helper base exp 1)))

(defn last-element [a-seq]
  (let [helper (fn [acc b-seq]
                 (if (empty? b-seq)
                   acc
                   (recur (first b-seq) (rest b-seq))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [acc s1 s2]
                 (if (or (empty? s1) (empty? s2))
                   (and acc (and (empty? s1) (empty? s2)))
                   (recur (and acc (== (first s1) (first s2))) (rest s1) (rest s2))))]
    (helper true seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [coll a-seq
         n 0]
    (cond (empty? coll) nil
      (pred (first coll)) n
      :else (recur (rest coll) (inc n))
    )))

(defn avg [a-seq]
  (loop [sum 0
         n 0
         coll a-seq]
    (cond (empty? coll) (/ sum n)
          :else (recur (+ sum (first coll)) (inc n) (rest coll)))))

(defn toggle [a-set a-key]
  (if (contains? a-set a-key)
    (disj a-set a-key)
    (conj a-set a-key)))

(defn parity [a-seq]
  (loop [a-set #{}
         coll a-seq]
    (cond
     (empty? coll) a-set
     :else (recur (toggle a-set (first coll)) (rest coll)))))

(defn fast-fibo [n]
  (cond (zero? n) n
        (<= n 2) 1
    :else (loop [fib-seq [1 0]
                 k 1]
            (if (== n k)
              (first fib-seq)
              (recur (cons (+ (first fib-seq) (first (rest fib-seq))) fib-seq) (inc k))))))

(defn cut-at-repetition [a-seq]
  (loop [a-set #{}
         res []
         b-seq a-seq]
    (cond
     (empty? b-seq) res
     (contains? a-set (first b-seq)) res
     :else (recur (conj a-set (first b-seq)) (conj res (first b-seq)) (rest b-seq)))))

