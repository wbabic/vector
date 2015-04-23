(ns vector.complex
  (:require [clojure.math.numeric-tower :as math]))

;; the field of complex numbers
;; where each number is represented as a vector
;; with addition and multiplication defined

(def zero [0 0])
(def one [1 0])
(def i [0 1])
(defn add [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])
(defn negate [[x y]]
  [(- x) (- y)])
(defn minus [z w]
  (add z (negate w)))
(defn mult [[x1 y1] [x2 y2]]
  [(- (* x1 x2) (* y1 y2))
   (+ (* x1 y2) (* x2 y1))])
(defn square [x] (* x x))
(defn det [[x y]] (+ (square x) (square y)))
(defn mult-inverse
  "when z not zero"
  [z]
  (assert (not (= zero z)))
  (let [[x y] z
        f (/ (det z))]
    [(* f x) (* f y -1)]))

(defn div
  "return z/w"
  [z w]
  (mult z (mult-inverse w)))

;; laws - check via test.check
;; associative law: (z1 * (z2 * z3)) = ((z1 * z2) * z3)
;; commutative z1 * z2 = z2 * z1
;; distributive z1 * (z2 + z3) = (z1 * z2) + (z1 * z3)

;; todo
;; associate x with [x 0]

;; modulus argument conjugate
(defn modulus-squared [z]
  (det z))
(defn modulus [z]
  (let [ds (modulus-squared z)]
    (math/sqrt ds)))
(defn argument [[x y]]
  (Math/atan2 y x))
(defn conjugate [[x y]]
  [x (* -1 y)])

;; properties
;; mod (z1 * z2) = mod z1 * mod z2
;; arg (z1 * z2) = arg z1 + arg z2
;; mod (z1 / z2) = mod z1 / mod z2
;; arg (z1 / z2) = arg z1 - arg z2
;; todo defone / -

;; real imaginary
(defn real [[x y]] x)
(defn imaginary [[x y]] y)

;; properties
;; z * zbar = (mod z) squared
;; real z = (z + zbar)/2
;; imaginary z = (z - zbar)/2
;; (z1 * z2) bar = z1 bar * z2 bar (also for + - /)
;; real z <= mod z
;; imag z <= mod z

;; triangle inequality
;; mod (z1 + z2) <= mod z1 + mod z2
;; proof

(defn pow
  "raise complex number z to the nth power
  where n is given ineger"
  [z n]
  (assert (integer? n) "n must be an integer")
  (loop [w z n n]
    (cond (zero? n) one
           (= 1 n) w
           :else (recur (mult w z) (dec n)))))

(defn powers
  "return list of powers of z from 0 to n"
  [z n]
  (let [f (partial pow z)]
    (map f (range 0 (inc n)))))

(comment
  (require '[vector.complex :as complex] :reload)
  (complex/add complex/one complex/i)
  ;;=> [1 1]
  (= complex/one (complex/mult-inverse complex/one))
  ;;=> true
  (complex/mult-inverse complex/i)
  ;;=> [0 -1]

  (let [w [(/ 2) (/ 2)]
        f (fn [z] (complex/mult w z))]
    (take 8 (iterate f w)))
  ;;=> ([1/2 1/2] [0N 1/2] [-1/4 1/4] [-1/4 0N] [-1/8 -1/8] [0N -1/8] [1/16 -1/16] [1/16 0N])

  (let [w [(/ 2) (/ 2)]]
    (for [n (range 1 9)]
      (reduce complex/add (complex/powers w n))))
  ;;=> ([3/2 1/2] [3/2 1N] [5/4 5/4] [1N 5/4] [7/8 9/8] [7/8 1N] [15/16 15/16] [1N 15/16])

  ;; (1 - z^(n+1))/(1 - z)
  (let [w [(/ 2) (/ 2)]
        f (fn [n] (complex/div (complex/minus complex/one (complex/pow w (inc n)))
                                 (complex/minus complex/one w)))]
    (map f (range 1 9)))
  ;;=> ([3/2 1/2] [3/2 1N] [5/4 5/4] [1N 5/4] [7/8 9/8] [7/8 1N] [15/16 15/16] [1N 15/16])
  )
