(ns util
  "Utility functions for solving project euler problems")

(defn primes [n]
  "Returns a list of all primes up to and including n"
  (if (< n 2)
    '()
    (let [n (int (inc n))]
      (let [root (int (Math/round (Math/floor (Math/sqrt n))))]
        (loop [i (int 3)
               a (int-array n)
               result (list 2)]
          (if (>= i n)
            (reverse result)
            (recur (+ i (int 2))
                   (if (<= i root)
                     (loop [arr a
                            inc (+ i i)
                            j (* i i)]
                       (if (>= j n)
                         arr
                         (recur (do (aset arr j (int 1)) arr)
                                inc
                                (+ j inc))))
                     a)
                   (if (zero? (aget a i))
                     (conj result i)
                     result))))))))

(defn prime? [n]
  "Returns whether a number is prime or not"
  (if (<= n 1)
    false
    (let [root (int (Math/round (Math/floor (Math/sqrt n))))]
      (loop [ps (primes root)]
        (if (= (count ps) 0)
          true
          (if (= (mod n (first ps)) 0)
            false
            (recur (next ps))))))))

(defn square [n]
  "Squares a number, ie raises it to the power 2"
  (* n n))

(defn square? [n]
  "Returns true if a number is a perfect square"
  (let [x (int (Math/floor (Math/sqrt n)))]
    (= (square x) n)))

(defn exp [b n]
  "Raise a number b to the power n"
  (if (= n 0)
    1
    (if (even? n)
      (square (exp b (/ n 2)))
      (* b (exp b (dec n))))))

(defn float-near-zero? [x]
  (or (and (pos? x) (< x 1e-6))
      (and (neg? x) (> x -1e-6))))

(defn near-zero? [x]
  "Returns true if x is either a number or rational and is 0, or is a float and is very near 0"
  (cond
    (float? x) (float-near-zero? x)
    :else (zero? x)))

(defn continued-fractions [r]
  "Returns a lazy sequence of the continued fractions which approximate r"
  (lazy-seq
    (let [i (int (Math/floor r))
          f (- r i)]
      (if (near-zero? f)
        (list i)
        (cons i (continued-fractions (/ f)))))))

(defn convergent [cf]
  "Returns the rational approximation of the continued fractions cf"
  (if (zero? (count cf))
    0
    (let [cf (reverse cf)]
      (loop [x (first cf)
             cf (rest cf)]
        (if (zero? (count cf))
          x
          (recur (+ (first cf) (/ x))
                 (rest cf)))))))
