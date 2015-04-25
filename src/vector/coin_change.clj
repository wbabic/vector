(ns vector.coin-change
  (:require [vector.generating :as g]
            [vector.fps :as fps]))

(def ones (g/geo 1))
(def fives (g/comp-zm 5 ones))
(def tens (g/comp-zm 10 ones))
(def twenty-fives (g/comp-zm 25 ones))
(def fifties (g/comp-zm 50 ones))

(def P ones)
(def N (fps/product fives P))
(def D (fps/product tens N))
(def Q (fps/product twenty-fives D))
(def C (fps/product fifties Q))

(comment
  (require '[vector.coin-change :as c] :reload)

  (take 10 c/ones)
  (take 10 c/fives)
  (take 11 c/tens)

  (take 10 c/P)
  (take 10 c/N)

  ;; the coefficient of z^50 is the 51st element in the sequence
  ;; the number of ways to make 50 cents
  ;; using all the coins
  (nth c/C 51)
  )
