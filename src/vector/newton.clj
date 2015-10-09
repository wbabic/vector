(ns vector.newton
  "Newton's method of calculating square roots")

(defn square [x]
  (* x x))

(defn abs [n]
  (max n (- n)))

(defn good-enough? [guess x]
  (< (abs (- (square guess) x)) 0.001))

(defn average [x y]
  (/ (+ x y) 2))

(defn improve [guess x]
  (average guess (/ x guess)))

(defn square-iter [guess x]
  (if (good-enough? guess x)
    guess
    (square-iter (improve guess x) x)))

(defn sqrt [x]
  (square-iter 1 x))

(comment
  (require '[vector.newton] :reload)
  (in-ns 'vector.newton)

  (double (sqrt 9))
  ;;=> 3.00009155413138

  )

;; ex 1.7
;; show that good-enough? is not good enough for
;; very small numbers
;; very big numbers
