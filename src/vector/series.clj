(ns vector.series
  (:require [clojure.math.numeric-tower :as math]))

;; sequence
;; series
;; geometric series

;; sequence of partial sums

;; first we define an infinite sequence, the natural numbers
(defn naturals
  "the sequence 0 1 2 3 4
note that this starts with 0"
  [] (range))

(comment
  (require '[vector.series :as s] :reload)
  (take 100 (s/naturals))
  ;;=> 0 1 .. 100
  )

(defn power
  "using math.numeric-tower"
  [x y]
  (math/expt x y))

;; 1 + a + a^2 + ...
(defn power-sequence [a]
  (map (fn [n] (power a n)) (naturals)))

;; 1 + a + ... + a^(n-1) = 1/(1-a) - a^n/(1-a)
;; (1-a)(1 + a + ... + a^(n-1)) = 1 - a^n
;; a = 1/2
;; 1 + 1/2 + ... + 1/2^n = 2 - 1/2^n
(defn partial-sum
  "the sum of the first n terms of given sequence"
  [seq n]
  (reduce + (take n seq)))

(defn sum-power-sequence
  "1/(1-a)"
  [a]
  (/ (- 1 a)))

(comment
  ;; geometrinc series
  (take 10 (s/power-sequence (/ 2)))
  ;;=> (1 1/2 1/4 1/8 1/16 1/32 1/64 1/128 1/256 1/512 1/1024)
  (take 10 (s/power-sequence (/ 3)))
  ;;=> (1 1/3 1/9 1/27 1/81 1/243 1/729 1/2187 1/6561 1/19683)

  (let [sn (partial s/partial-sum (s/power-sequence (/ 2)))]
    (map sn (take 10 (s/naturals))))
  ;;=> (0 1 3/2 7/4 15/8 31/16 63/32 127/64 255/128 511/256)

  (let [sn (partial s/partial-sum (s/power-sequence (/ 3)))]
    (map sn (take 10 (s/naturals))))
  ;;=> (0 1 4/3 13/9 40/27 121/81 364/243 1093/729 3280/2187 9841/6561)

  (s/sum-power-sequence (/ 2))

  (s/sum-power-sequence (/ 3))
  (let [s1 (s/power-sequence (/ 3))]
    (- (s/sum-power-sequence (/ 3)) (s/partial-sum s1 10)))
  ;;=> 1/39366
  )
