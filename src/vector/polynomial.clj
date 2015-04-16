(ns vector.polynomial)

(defn to-string
  "return a string represnetaion of a polynomial
  in given variable"
  [poly variable]
  (let [var-pow (fn [pow]
                  (cond
                    (zero? pow) ""
                    (= 1 pow) variable
                    :else (str variable "^" pow)))
        pr-item (fn [item]
                  (cond
                    (zero? item) ""
                    (= 1 item) ""
                    :else item))]
    (clojure.string/join " + "
                         (reverse
                          (map-indexed
                           (fn [index item] (str (pr-item item) (var-pow index)))
                           poly)))))

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
