(ns vector.rational
  "explore the space of ration numbers"
  (:require [vector.number :as num]
            [vector.integer :as int]
            [vector.euclid :as euc :refer [gcd]]))

(comment
  (require '[vector.rational] :reload)
  (in-ns 'vector.rational)
  (use 'clojure.repl)
  )

(declare rational)

(defrecord Rational [numerator denominator]
  num/Addition
  (num/add [_ y]
    (let [n1 numerator
          d1 denominator
          n2 (:numerator y)
          d2 (:denominator y)]
      (rational (+ (* n1 d2) (* n2 d1))
                  (* d1 d2))))
  (num/negative [x] (rational (- numerator) denominator))
  (num/zero? [_] (zero? numerator))

  num/Equality
  (num/equals? [_ y]
    (let [n1 numerator
          d1 denominator
          n2 (:numerator y)
          d2 (:denominator y)]
      (and (== n1 n2) (== d1 d2))))
  )

(defn rational
  ([n] (rational n 1))
  ([n d]
   (let [g (gcd n d)]
     (if (and (> n 0) (< d 0))
       (->Rational (/ (- n) g) (/ (- d) g))
       (->Rational (/ n g) (/ d g))))))

(def half (rational 1 2))
(def third (rational 1 3))
(def fourth (rational 1 4))
(def fifth (rational 1 5))

(comment
  (rational 1 2)
  (num/negative (rational 1 2))
  (num/zero? (->Rational 0 10))
  (num/equals? half (rational 2 4))
  )
