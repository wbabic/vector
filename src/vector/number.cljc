(ns vector.number
  "number protocols and properties
  addition multiplication identities inverses"
  (:refer-clojure :exclude [zero?])
  (:require [clojure.core :as core]))

(defprotocol Addition
  (add [x y])
  (negative [x])
  (zero? [x]))

(defprotocol Multiplication
  (multiply [x y])
  (reciprocal [z])
  (one? [x]))

(defprotocol Equality
  (equals? [x y]))

(defprotocol Evaluate
  (evaluate [x]))

(defprotocol Conjugate
  (conjugate [x]))

(comment
  (require '[vector.number] :reload)
  (in-ns 'vector.number)
  (use 'clojure.repl)
  )

;; properties:
;; adding a number to its negative yields zero
;; multiplying a number by its reciprocal yields its inverse
;; commutativity
;; associativity
;; distributive
