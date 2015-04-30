(ns vector.hypergeometric
  (:require [vector.factorial :as fac]
            [clojure.math.numeric-tower :as math]))

;; hypergeometric functions

;; 2F1(a,b;c;z) = ((a)n (b)n) / (c)n  z^n/n!
;; (a)n = Pochhammer Symbol = rising factorial
;; (fac/rising-factorial)

;; special cases
;; ln(1+z)
;; (1-z)^-a
;; arcsin(z)
;; e^z

;; Legendre, Chebyshev, Gegenbauer polynomials
;; see wikipedia hypergeometric function
