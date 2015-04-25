(ns vector.fps
  (:require [vector.factorial :as fac]
            [vector.fibonacci :as f]))

;; ideas from Henrici
;; Applied and Computational Complex Analysis, Vol 1

;; formal power series
;; an infinate sequence
;; or a mapping from the nonnegative integers
;; into a field, usually the complex numbers

;; represented as a power series
;; a0 + a1 x + a2 x^2 + a3 x^3 + ...

;; examples
;; 1 1/1! 1/2! 1/3! ...
;; 1 1! 2! ...

(def zero (repeat 0))
(def I (lazy-cat [1] zero))
(def X (lazy-cat [0 1] zero))

(defn add [p1 p2]
  (map + p1 p2))

;; 1 1 1 1  ....
(def p (repeat 1))
;; 1 2 3 ...
(def q (iterate inc 1))

(defn nth-term [p1 p2 n]
  (reduce + (for [k (range (inc n))] (* (nth p1 k) (nth p2 (- n k))))))

(defn product [p1 p2]
  (let [f (fn [n] (nth-term p1 p2 n))]
    (map f (range))))

(comment
  (require '[vector.fps :as fps] :reload)
  (require '[vector.factorial :as fac] :reload)
  (take 5 fps/zero)
  (take 5 fps/I)
  (take 5 fps/X)
  (def p1 (map fac/factorial (range)))
  (def p2 (map #(/ (fac/factorial %)) (range)))
  (take 10 p1)
  (take 10 p2)
  (take 10 (fps/add fps/p1 fps/p2))
  (nth fps/p2 5)
  (take 10 (fps/product fps/p fps/q))

  )

(comment
  ;; exercises
  ;; 1. find inverse of 1 - x
  (let [P (lazy-cat [1 -1] (repeat 0))
        Q (repeat 1)
        PQ (fps/product P Q)
        QP (fps/product Q P)]
    [(take 10 PQ) (take 10 QP)])

  ;; 3. P = fib fps
  ;; reciprocal P = (1 -1 -1 0 0 0) = 1 -x -x^2
  (let [P (f/fibs)
        Q (lazy-cat [1 -1 -1] (repeat 0))
        PQ (fps/product P Q)
        QP (fps/product Q P)]
    [(take 10 P)
     (take 10 PQ)
     (take 10 QP)])

  ;; 4. P = 1/1! + 1/2! x + 1/3! x^2
  (let [P (map #(/ (fac/factorial (inc %))) (range))]
    (take 10 P))

  )
