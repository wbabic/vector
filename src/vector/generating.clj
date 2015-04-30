(ns vector.generating
  (:require [vector.factorial :as fac]
            [clojure.math.numeric-tower :as math]))

(def zero (repeat 0))
(def one (lazy-cat [1] zero))
(def nats (iterate inc 1))
(def non-negs (lazy-cat [0] nats))

;; z^m
;; 0 0 ... 0 1 0 ...
;; a sequence with a 1 in the mth place and zeros elsewhere
(defn zm [m] (lazy-cat (take m zero) one))

;; 1/(1-cz)
;; 1 c c^2 c^3 ...
(defn geo
  "geometric series"
  [c]
  (iterate #(* c %) 1))

;; binomial series
;; (1 + kz)^c = (choose c n)
(defn binomial
  "(1+z)^c"
  ([c]
   (let [xf (map #(fac/choose c %))]
     (sequence xf non-negs)))
  ([k c]
   (let [xf (map #(* (fac/choose c %)
                     (math/expt k %)))]
     (sequence xf non-negs))))

;; specific cases of binomial theorem
;; 1/(1-z)^c = (binomial -1 c)
;; 1/(1 + kz)^c = (binomial k c)
;; 1/(1-z)^2 =  1 2 3 4 5 ... = (binomial -1 -2)

;; compose a sequence with z^m
;; G(z^m)
(defn interleave-m-zeros
  ([m]
   (mapcat #(conj (repeat m 0) %)))
  ([m g]
   (mapcat #(conj (repeat m 0) %) g)))

;; G(z^m)
(defn comp-zm
  ([m]
   (interleave-m-zeros (dec m)))
  ([m g]
   (interleave-m-zeros (dec m) g)))

;; exponential
;; e^z
;; 1/0! 1/1! 1/2! 1/3! ...
(def e (sequence
        (map #(/ 1 (fac/factorial %)))
        non-negs))

;; ln 1/(1-z)
;; (0 1 1/2 1/3 1/4 ...)
(def ln-1 (lazy-cat [0] (map #(/ 1 %) nats)))
;; ln(1+z)
;; (0 1 -1/2 1/3 -1/4 ...)
(def ln-2 (lazy-cat [0] (map #(/ (math/expt -1 (inc %)) %) nats)))

(comment
  (require '[vector.generating :as g] :reload)
  (take 10 g/zero)
  (take 10 g/one)
  (take 10 (g/zm 1))
  (take 10 (g/zm 4))

  ;; geometric series 1/(1 - cz)
  ;; for various values of c
  (take 10 (g/geo 1))
  ;;=> [1 1 1 1 1 1 1 1 1 1]
  (take 10 (g/geo -1))
  ;;=> [1 -1 1 -1 1 -1 1 -1 1 -1]
  (take 10 (g/geo (/ 2)))
  ;;=> [1 1/2 1/4 1/8 1/16 1/32 1/64 1/128 1/256 1/512]
  (take 10 (g/geo 2))
  ;;=> [1 2 4 8 16 32 64 128 256 512]
  (take 10 (g/geo (/ 3)))
  ;;=> [1 1/3 1/9 1/27 1/81 1/243 1/729 1/2187 1/6561 1/19683]
  (take 10 (g/geo-2 (/ 3)))

  (take 10 g/nats)
  ;;=> [1 2 3 4 5 6 7 8 9 10]

  (take 10 g/e)
  ;;=> [1 1 1/2 1/6 1/24 1/120 1/720 1/5040 1/40320 1/362880]

  (take 10 g/ln-1)
  ;;=>(0 1 1/2 1/3 1/4 1/5 1/6 1/7 1/8 1/9)

  (take 10 g/ln-2)
  ;;=> (0 1 -1/2 1/3 -1/4 1/5 -1/6 1/7 -1/8 1/9)

  ;; binomial series (1 + z)^n
  (take 10 (g/binomial 4))
  ;;=> [1 4 6 4 1 0 0 0 0 0]

  ;; (1 + z)^(1/2)
  (take 10 (g/binomial (/ 2)))
  ;;=> [1 1/2 -1/8 1/16 -5/128 7/256 -21/1024 33/2048 -429/32768 715/65536]

  ;;=> 1/(1+z)^2
  (take 10 (g/binomial (/ -2)))
  ;;=> [1 -1/2 3/8 -5/16 35/128 -63/256 231/1024 -429/2048 6435/32768 -12155/65536]

  ;;=> 1/(1-z)
  (take 10 (g/binomial -1 -1))
  ;;=> [1 1 1 1 1 1 1 1 1 1]

  ;;=> 1/(1+z)
  (take 10 (g/binomial -1))
  ;;=> [1 -1 1 -1 1 -1 1 -1 1 -1]

  ;; (1-z)^(1/2)
  (take 10 (g/binomial -1 (/ 2)))
  ;;=> [1 -1/2 -1/8 -1/16 -5/128 -7/256 -21/1024 -33/2048 -429/32768 -715/65536]

  ;;=> 1/(1-z)^2
  (take 10 (g/binomial -1 -2))
  ;;=> [1 2 3 4 5 6 7 8 9 10]

  ;;=> 1/(1-z^2)
  ;;=> [1 0 1 0 1 0 1 0]
  (take 10 (g/interleave-m-zeros 1 (g/geo 1)))
  ;;=> [1 0 1 0 1 0 1 0 1 0]

  ;; 1/(1 - (z^m)) = (g/interleave-m-zeros (dec m) (g/geo 1))
  (take 10 (g/comp-zm 4 (g/binomial -1 -1)))
  (take 10 (g/comp-zm 4 g/e))
  ;;=> [1 0 0 0 1 0 0 0 1/2 0]

  ;;=> 1/(1-z^m)
  ;;=> [1 0 0 ... 0 1 0 0 ... 0 1 ...]

  (into [] (comp (g/comp-zm 4) (take 10)) (g/binomial -1 -2))
  ;;=> [1 0 0 0 2 0 0 0 3 0]
  )
