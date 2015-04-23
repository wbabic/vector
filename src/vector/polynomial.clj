(ns vector.polynomial)

(defn to-string
  "return a string represnetaion of a polynomial
  in given variable, defaults to x"
  ([poly] (to-string poly "x"))
  ([poly variable]
   (let [var-pow (fn [coefficient pow]
                   (cond
                     (zero? coefficient) nil
                     (zero? pow) ""
                     (= 1 pow) variable
                     :else (str variable "^" pow)))
         pr-coefficient (fn [coefficient exponent]
                   (cond
                     (zero? coefficient) nil
                     (= 1 coefficient) (if (zero? exponent) "1"  "")
                     :else coefficient))]
     (clojure.string/join " + "
                          (filter (comp not empty?)
                                  (map-indexed
                                   (fn [exponent coefficient]
                                     (str (pr-coefficient coefficient exponent)
                                          (var-pow coefficient exponent)))
                                   poly))))))

(def zero-poly [])
(defn zero-poly? [poly] (empty? poly))
(def unit-poly [1])

(defn to-proper
  "return a proper polynomial - with trailing zeros removed"
  [poly]
  (assert (vector? poly) "poly must be a vector")
  (if (> (count poly) 0)
    (if (zero? (peek poly))
      (to-proper (pop poly))
      poly)
    poly))

(defn add
  "add two polynomials"
  [p1 p2]
  (let [l1 (count p1)
        l2 (count p2)
        l (max l1 l2)]
    (to-proper
     (vec (for [i (range l)]
            (+ (get p1 i 0) (get p2 i 0)))))))

(defn scal-mult
  "multiply a polynomial by a scalar"
  [scalar poly]
  (to-proper (vec (for [term poly] (* scalar term)))))

(defn mult-by-x
  "multiply polynomial by x"
  [poly]
  (if (zero-poly? poly) zero-poly
      (into [0] poly)))

(defn mult
  "multiply two polynomials"
  [p1 p2]
  (if (zero-poly? p1)
    zero-poly
    (add (scal-mult (first p1) p2)
         (mult-by-x (mult (rest p1) p2)))))

(defn mult-recur
  "multiply two polynomials"
  [p1 p2]
  (if (zero-poly? p1)
    zero-poly
    (add (scal-mult (first p1) p2)
         (mult (mult-by-x (rest p1)) p2))))

(defn pow
  "raise poly to the nth power"
  [poly n]
  (loop [p poly n n]
    (cond (zero? n) unit-poly
          (= 1 n) p
          :else (recur (mult p poly) (dec n)))))

(defn eval-poly
  "evaluate a ploynomial at given number"
  [poly number]
  )

(comment
  (require '[vector.polynomial :as poly] :reload)

  ;; [5 17 6] -> 5 + 17x + 6x^2

  (poly/scale-poly 3 [1 2 3])
  (poly/scal-mult 0 [1 2 3])
  (poly/add [1 2 3] [2 1 -3])
  (poly/mult-by-x [1 2 3])
  (poly/mult [1 3] [5 -2 1])
  (poly/mult [5 -2 1] [1 3])
  (poly/mult [2 -1 1] [-2 1 1])
  (poly/mult [1 1] [1 1])
  (poly/mult [1 1] (poly/mult [1 1] [1 1]))
  (poly/mult [3 1 1] [2 1])
  (poly/to-string
   (poly/mult [1 1] [3 1 1]) "s")
  (poly/to-string
   (poly/mult [3 1] [4 1 1]) "q")
  (poly/to-string
   (poly/mult [4 -2 -2] [2 3]) "t"))


(comment
  ;; 2 dice
  (def d [0 1 1 1 1 1 1])
  (poly/mult d d)
  (poly/to-string (poly/mult d d))
  ;;=> "x^12 + 2x^11 + 3x^10 + 4x^9 + 5x^8 + 6x^7 + 5x^6 + 4x^5 + 3x^4 + 2x^3 + x^2"
  (get (poly/mult d d) 6)
  ;;=> 5

  (def p1 [1 1 1 1 1 1 1 1 1 1 1])
  (def p2 [1 0 1 0 1 0 1 0 1 0 1])
  (poly/to-string p1)
  (poly/to-string p2)
  (poly/to-string (poly/mult p1 p2))

  (def p3 [1 1 1 1 1 1 1 1 1 1 1])
  (poly/to-string p3)
  (poly/to-string (poly/mult p3 p3))

  ;; binomial cooficients
  ;; (1 + x)^n
  (poly/pow [1 1] 2)
  (time (poly/pow [1 1] 20))
  ;;=> "Elapsed time: 51.465886 msecs"
  ;; see pascal and factorial
  (require '[vector.pascal :as p] :reload)
  (require '[vector.factorial :as fac] :reload)
  (time (let [n 20] (vec (for [k (range (inc n))] (p/C n k)))))
  ;;=> "Elapsed time: 17.739452 msecs"
  ;;=> "Elapsed time: 1.537301 msecs"
  (time (let [n 20] (vec (for [k (range (inc n))] (fac/choose n k)))))
  ;;=> "Elapsed time: 15.662246 msecs"
  ;;=> "Elapsed time: 1.582086 msecs"
  (time (let [n 49] (vec (for [k (range (inc n))] (p/C n k)))))
  ;;=> "Elapsed time: 1.732413 msecs"
  ;;=> "Elapsed time: 1.00644 msecs"
  (time (let [n 49] (vec (for [k (range (inc n))] (fac/choose n k)))))
  ;;=> "Elapsed time: 25.28586 msecs"
  ;;=> "Elapsed time: 11.117085 msecs"
  )
