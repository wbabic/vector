(ns vector.complex)

;; the field of complex numbers
;; where each number is represented as a vector
;; with addition and multiplication defined

(def zero [0 0])
(def one [1 0])
(def i [0 1])
(defn add [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])
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
    (Math/sqrt ds)))
(defn argument [[x y]]
  (Math/atan2 (/ y x)))
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

(comment
  (require '[vector.complex :as complex] :reload)
  (complex/add complex/one complex/i)
  ;;=> [1 1]
  (= complex/one (complex/mult-inverse complex/one))
  ;;=> true
  (complex/mult-inverse complex/i)
  ;;=> [0 -1]
  )
