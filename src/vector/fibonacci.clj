(ns vector.fibonacci
  (:require [vector.transduce :as t]))

(defn fibs
  "return the sequence of Fibonacci numbers"
  []
  (letfn [(fib [[a b]] [b (+' a b)])]
    (->> [0 1]
         (iterate fib)
         (map first))))

(defn square [n] (* n n))

(defn sum-fibs-squared [n]
  (reduce + (take n (map square (fibs)))))

;; represent as an extension of rationals
;; 1/2(1 +- sqrt(5))
;; a + b * sqrt-5 as a vector [a b]

(def one [1 0])
(def rt5 [0 1])

(defn as-decimal [[a b]]
  (+ a (* b (Math/sqrt 5))))

(defn evaluate [n]
  (as-decimal n))

;; add, multiply
;; take nth powers
;; evaluate in a polynomial
(defn add [[a1 b1] [a2 b2]]
  [(+ a1 a2) (+ b1 b2)])

(defn negative [[a1 a2]]
  [(- a1) (- a2)])

(defn minus [a b]
  (add a (negative b)))

;; (= 5 (* rt5 rt5))
(defn mult [[a1 b1] [a2 b2]]
  [(+ (* a1 a2) (* 5 b1 b2))
   (+ (* a1 b2) (* a2 b1))])

(defn reciprocal
  "return 1/a"
  [[a1 b1]]
  (let [f (- (* a1 a1) (* 5 b1 b1))]
    [(/ a1 f) (/ (- b1) f)]))

(defn conjugate [[a b]]
  [a (- b)])

(defn divide
  "a/b"
  [a b]
  (mult a (reciprocal b)))

(defn pow
  "raise to the nth power"
  [a n]
  (loop [b a n n]
    (cond (zero? n) one
          (= 1 n) b
          :else (recur (mult b a) (dec n)))))

(def alpha [(/ 2) (/ 2)])
(def Phi alpha)
(def phi (reciprocal Phi))
(def beta (conjugate alpha))

(defn fib
  "the nth fibonacci"
  [n]
  (let [an (pow alpha n)
        bn (pow beta n)
        f (reciprocal rt5)]
    (mult f (minus an bn))))

(def eq-1
  (fn [x] (- (* x x) x 1)))

(def eq-2
  (fn [x] (+ (* x x) x -1)))

(comment
  (require '[vector.fibonacci] :reload)
  (in-ns 'vector.fibonacci)

  (take 15 (fibs))
  (take 15 (map square (fibs)))
  ;;=> (0 1 1 4 9 25 64 169 441 1156 3025 7921 20736 54289 142129)
  (map sum-fibs-squared (range 1 11))
  ;;=> (0 1 2 6 15 40 104 273 714 1870)

  ;; now look at the product of two adjacent fibs
  (take 10 (partition 2 1 (fibs)))
  (->> (take 10 (partition 2 1(fibs)))
       (map vec)
       (map (fn [[f1 f2]] (* f1 f2))))
  ;;=> (0 1 2 6 15 40 104 273 714 1870)
  (add alpha beta)
  ;;=> [1N 0N]
  (mult alpha beta)
  ;;=> [-1N 0N]

  (= one (mult Phi phi) (minus Phi phi))
  (= phi
     (minus Phi one)
     (reciprocal Phi))
  (= (negative phi) beta)
  (= beta (conjugate alpha))
  (= (reciprocal alpha)
     (minus alpha one)
     (negative beta))
  ;;=> true

  ;; solutions to quadratic equations
  (eq-1 (evaluate Phi))
  (eq-1 (evaluate beta))
  (eq-2 (evaluate phi))
  (eq-2 (evaluate (negative Phi)))
  ;;=> 0.0

  ;; pow
  (= (pow alpha 2) (mult alpha alpha))
  (= (pow alpha 3) (mult alpha (mult alpha alpha)))

  (map (fn [[f1 f2]] (/ f1 f2)) (map vec (take 100 (partition 2 1 (fibs)))))

  (map (fn [[f1 f2]] (double (/ f1 f2))) (map vec (take 20 (partition 2 1 (fibs)))))

  (let [t (evaluate phi)
        f (fn [[f1 f2]] (double (/ f1 f2)))
        g (fn [x] (Math/abs (- t x)))]
    (->> (fibs)
         (partition 2 1)
         (take 35)
         (map vec)
         (map f)
         (map g)))

  (let [t (evaluate phi)
        f (fn [[f1 f2]] (double (/ f1 f2)))
        g (fn [x] (Math/abs (- t x)))
        epsilon 1e-15]
    (first
     (->> (fibs)
          (partition 2 1)
          (map vec)
          (map f)
          (map g)
          (drop-while #(> % epsilon)))))
  )
