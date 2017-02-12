(ns looping-is-recursion)

(defn power
  "Exponentiation with use of tail recursion."
  [base exp]
  (let [evaluator (fn [acc base exp]
    ;; Keeping single if statement instead of cond makes this slightly quicker.
                      (if (== 1 exp) acc (recur (* acc base) base (dec exp))))]
    (cond (zero? exp) 1 (== 1 exp) base :else (evaluator base base exp))))

(defn singleton?
  "Find out, if this collection contains only one item in."
  [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn last-element
  "Get last element from a sequence."
  [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (recur (rest a-seq))))

(defn seq=
  "Compare two sequences for equality."
  [seq1 seq2]
  (let [first-empty (empty? seq1) second-empty (empty? seq2)]
    (cond
      (and first-empty second-empty) true
      (or first-empty second-empty) false
      (not= (first seq1) (first seq2)) false
      :else (recur (rest seq1) (rest seq2)))))

(defn find-first-index
  "Find first index in a sequence, where predicate evaluates truthy."
  [pred a-seq] (let [last-index (count a-seq)]
    (loop [current-index 0]
        (cond
          (== current-index last-index) nil
          (pred (get a-seq current-index)) current-index
          :else (recur (inc current-index))))))

(defn avg
  "Compute the average of a sequence."
  [a-seq] (let [num-items (count a-seq)]
    (loop [ctr 0 acc 0] (if
      (== ctr num-items) (/ acc ctr)
      (recur (inc ctr) (+ acc (get a-seq ctr)))))))

(defn parity
  "Return set of those elements, which occur odd number of times in a sequence."
  [a-seq] (let [num-items (count a-seq)]
    (loop [odds #{} idx 0]
      (if (== idx num-items) odds
        (let [item (get a-seq idx)
              set-operator (if (contains? odds item) disj conj)]
              (recur (set-operator odds item) (inc idx)))))))

(defn fast-fibo
  "A fibonacci implementation with use of loop and recur."
  [n]
  (loop [prev 1 acc 0 ctr 0] (if (== ctr n) acc
          (recur acc (+ acc prev) (inc ctr)))))

(defn cut-at-repetition
  "Return elements from the sequence, up to first repetition."
  [a-seq] (let [num-items (count a-seq)]
    (loop [seen #{} res [] ctr 0] (if (== num-items ctr) res
      (let [item (get a-seq ctr) next-ctr (inc ctr)]
        (if (contains? seen item)
          (recur seen res next-ctr)
          (recur (conj seen item) (conj res item) next-ctr)))))))
