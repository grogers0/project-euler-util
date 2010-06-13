(ns util
  "Utility functions for solving project euler problems")

(defn gen-primes [n]
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
      (loop [primes (gen-primes root)]
        (if (nil? (first primes))
          true
          (if (= (mod n (first primes)) 0)
            false
            (recur (next primes))))))))

(defn square [n]
  "Squares a number, ie raises it to the power 2"
  (* n n))

(defn square? [n]
  "Returns true if a number is a perfect square"
  (let [x (bigint (Math/round (Math/sqrt n)))]
    (= (square x) n)))

(defn pow [b n]
  "Raise a number b to the power n"
  (if (= n 0)
    1
    (if (even? n)
      (square (pow b (/ n 2)))
      (* b (pow b (dec n))))))

(defn abs [x]
  "Returns the absolute value of x"
  (if (pos? x)
    x
    (- x)))

(defn float-near-zero? [x]
  (< (abs x) 1e-6))

(defn near-zero? [x]
  "Returns true if x is either a number or rational and is 0, or is a float and is very near 0"
  (if (float? x)
    (float-near-zero? x)
    (zero? x)))

(defn nearly-equal? [a b]
  "Returns true if a and b are very close to each other relative to their absolute value"
  (if (near-zero? b)
    (near-zero? a)
    (< (abs (/ (- a b) b)) 1e-6)))

(defn countmap
  "With no arguments creates an empty countmap, with one or two arguments
  creates a countmap with ks as its keys and optionally vs as its vals,
  if vs are not specified, they will be 1"
  ([] {})
  ([ks] (countmap ks (repeat 1)))
  ([ks vs] (apply hash-map (interleave ks vs))))

(defn countmap-insert
  "Increments the count of the item in the countmap by n, or if n is not
  present, by 1 and returns this new countmap"
  ([coll item] (countmap-insert coll item 1))
  ([coll item n] (merge-with + coll {item n})))

(defn continued-fractions [r]
  "Returns a lazy sequence of the continued fractions which approximate r"
  (lazy-seq
    (let [i (int (Math/floor r))
          f (- r i)]
      (if (near-zero? f)
        (list i)
        (cons i (continued-fractions (/ f)))))))

(defn continued-fractions-sqrt
  "Returns a lazy sequence of the continued fractions which approximate (sqrt n)"
  ([n] (continued-fractions-sqrt n 0 1))
  ([n a b]
   (lazy-seq
     (if (square? n)
       (list (int (Math/round (Math/sqrt n))))
       (let [m (int (/ (+ (Math/sqrt n) a) b))
             anext (+ (- a) (* m b))
             bnext (/ (+ n
                         (- (square a))
                         (* 2 a m b)
                         (- (* (square m) (square b))))
                      b)]
         (cons m (continued-fractions-sqrt n anext bnext)))))))

(defn convergent [cf]
  "Returns the rational approximation of the continued fractions cf"
  (if (nil? (seq cf))
    0
    (let [cf (reverse cf)]
      (loop [x (first cf)
             cf (next cf)]
        (if (nil? cf)
          x
          (recur (+ (first cf) (/ x))
                 (next cf)))))))

(defn prime-factors [n]
  "Returns a map of the prime factors of n, with the keys being the factors
  and the values being the number of times that factor is repeated"
  (loop [n n
         factors (countmap)]
    (if (<= n 1)
      factors
      (let [root (int (Math/round (Math/sqrt n)))
            factor (loop [p 2, primes (next (gen-primes root))]
                     (cond (= (mod n p) 0) p
                       (empty? primes) n
                       :else (recur (first primes) (next primes))))]
        (recur (/ n factor) (countmap-insert factors factor))))))

(defn number-from-factors [factors]
  "Returns the number made by the prime factors given by factors, where factors
  is a map with keys as the factors and values as the number of times that
  factor appears"
  (reduce (fn [acc fact]
            (let [p (key fact), k (val fact)]
              (* acc (pow p k))))
          1 factors))

(defn totient-from-factors [factors]
  "Returns Euler's totient (phi function) of the number made by the prime
  factors given by factors, where factors is a map with keys as the factors and
  values as the number of times that factor appears"
  (if (nil? (first factors))
    0
    (reduce (fn [acc fact]
              (let [p (key fact), k (val fact)]
                (* acc (dec p) (pow p (dec k)))))
            1 factors)))

(defn totient [n]
  "Returns Euler's totient (phi function) of n - the number of integers less
  than n which is coprime to n"
  (totient-from-factors (prime-factors n)))

(defn gcd
  "Returns the greatest common divisor of multiple integers"
  ([a b] (if (zero? b)
           a
           (gcd b (- a (* b (bigint (/ a b)))))))
  ([a b & more] (reduce gcd (gcd a b) more)))

(defn lcm
  "Returns the least common multiple of multiple integers"
  ([a b] (* a b (/ (gcd a b))))
  ([a b & more] (reduce lcm (lcm a b) more)))

(defn coprime?
  "Returns if the input integers are coprime (relatively prime), ie. their
  greatest common divisor is 1"
  ([a b & more] (= (apply gcd a b more) 1)))

(defn digit-seq [n]
  "Returns a lazy sequence that iterates in reverse order over the decimal
  digits in an integer"
  (lazy-seq
    (let [n (bigint n)]
      (if (zero? n)
        nil
        (cons (mod n 10) (digit-seq (.divide n (bigint 10))))))))

(defn factorial [n]
  "Returns the factorial of an integer"
  (reduce * 1 (range 2 (inc n))))

(defn zip
  "lazily extracts one item at a time from the sequences and conj's them into
  the data structure given by ds"
  ([ds aseq] (lazy-seq
               (if (nil? (seq aseq))
                 nil
                 (cons (conj ds (first aseq)) (zip ds (rest aseq))))))
  ([ds aseq & more] (lazy-seq
                      (if-not (not-any? #(nil? (seq %)) (cons aseq more))
                        nil
                        (cons (into ds (map first (cons aseq more)))
                              (apply zip ds (rest aseq) (map rest more)))))))

(defn unique
  "Returns a lazy sequence with consecutive elements in coll removed, where
  the comparison is done either with pred, or = if pred is not supplied"
  ([coll] (unique = coll))
  ([pred coll] (lazy-seq
                 (if-let [s (seq coll)]
                   (cons (first s) (unique (drop-while #(pred % (first s)) s)))
                   coll))))
