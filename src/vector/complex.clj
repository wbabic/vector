(ns vector.complex
  (:require [vector.number :as num]
            [vector.root :as rat]))

(comment
  (require '[vector.complex] :reload)
  (in-ns 'vector.complex)
  (use 'clojure.repl)
  )

(defn evaluate
  [x]
  (if (rational? x) x
      (num/evaluate x)))

(defn add
  "add two numbers
  where a number is rational or a rational root"
  [x y]
  (if (and (rational? x)
           (rational? y))
    (+ x y)
    (if (rat/root? x)
      (num/add x y)
      (num/add y x))))

(defn negative
  [x]
  (if (rational? x) (- x) (num/negative x)))

(defn multiply
  "multiply two numbers
  where a number is rational or a rational root"
  [x y]
  (if (and (rational? x)
           (rational? y))
    (* x y)
    (if (rat/root? x)
      (num/multiply x y)
      (num/multiply y x))))

(defn reciprocal
  [x]
  (if (rational? x) (/ x) (num/reciprocal x)))

(defn is-equal?
  [x y]
  (cond
    (and (rational? x) (rational? y))
    (== x y)

    (rat/root? x) (num/equals? x y)

    :else (num/equals? y x)))

(declare complex)

(defrecord Complex [x y]
  num/Evaluate
  (num/evaluate [_]
    (mapv evaluate [x y]))

  num/Addition
  (num/add
    [_ z]
    (complex (add x (:x z))
             (add y (:y z))))
  (num/negative
    [_]
    (complex (negative x) (negative y)))
  (num/zero?
    [_]
    (and (if (rational? x) (zero? x) (num/zero? x))
         (if (rational? y) (zero? y) (num/zero? y))))

  num/Equality
  (num/equals? [_ z]
    (and (is-equal? x (:x z))
         (is-equal? y (:y z))))

  num/Conjugate
  (num/conjugate [_]
    (complex x (negative y)))

  num/Multiplication
  (num/multiply [_ z]
    (let [mx (add (multiply x (:x z)) (negative (multiply y (:y z))))
          my (add (multiply x (:y z)) (multiply y (:x z)))]
      (complex mx my)))
  (num/reciprocal [_]
    (let [k (reciprocal (add (multiply x x) (multiply y y)))]
      (complex (multiply k x) (multiply k (negative y)))))
  (num/one? [this]
    (and (if (rational? x) (== x 1) (num/one? x))
         (if (rational? y) (== y 0) (num/zero? y)))))

(defn complex
  ([x] (complex x 0))
  ([x y] (->Complex x y)))

(def zero (complex 0))
(def one (complex 1 0))
(def i (complex 0 1))
(def omega (complex (/ 2) (rat/root 3 (/ 2))))
