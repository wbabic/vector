(ns vector.generating
  (:require [vector.factorial :as fac]
            [clojure.math.numeric-tower :as math]))

;; explore generating functions using lazy sequnces and transducers

;; some basic generating functions and their sequences

;; first some sequences
(def zero (repeat 0))
(def one (lazy-cat [1] zero))
(def nats (iterate inc 1))
(def non-negs (lazy-cat [0] nats))

;; take first m transducer
(defn take-m [m] (take m))
(def take-ten (take 10))

;; z^m
;; 0 0 ... 0 1 0 ...
;; a sequence with a 1 in the mth place and zeros elsewhere
(defn zm [m] (lazy-cat (take m zero) one))


;; 1/(1-z) = (binomial -1 1)
;; a sequence with all ones
(def zn (repeat 1))

;; note: zn = (geo 1)
;; 1/(1-cz)
;; 1 c c^2 c^3 ...
(defn geo
  "geometric series"
  [c]
  (iterate #(* c %) 1))

(defn mult-by-c [c]
  (map #(* c %)))

(defn geo-2
  ""
  [c]
  (sequence (mult-by-c c) zn))

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

(comment
  (require '[vector.generating :as g] :reload)
  (into [] g/take-ten g/zero)
  (into [] g/take-ten g/one)
  (into [] g/take-ten (g/zm 1))
  (into [] g/take-ten g/zn)

  ;; geometric series 1/(1 - cz)
  ;; for various values of c
  (into [] g/take-ten (g/geo 1))
  ;;=> [1 1 1 1 1 1 1 1 1 1]
  (into [] g/take-ten (g/geo -1))
  ;;=> [1 -1 1 -1 1 -1 1 -1 1 -1]
  (into [] g/take-ten (g/geo (/ 2)))
  ;;=> [1 1/2 1/4 1/8 1/16 1/32 1/64 1/128 1/256 1/512]
  (into [] g/take-ten (g/geo 2))
  ;;=> [1 2 4 8 16 32 64 128 256 512]
  (into [] g/take-ten (g/geo (/ 3)))
  ;;=> [1 1/3 1/9 1/27 1/81 1/243 1/729 1/2187 1/6561 1/19683]
  (into [] g/take-ten (g/geo-2 (/ 3)))

  (into [] g/take-ten g/nats)
  ;;=> [1 2 3 4 5 6 7 8 9 10]

  (into [] g/take-ten g/e)
  ;;=> [1 1 1/2 1/6 1/24 1/120 1/720 1/5040 1/40320 1/362880]

  ;; binomial series (1 + z)^n
  (into [] g/take-ten (g/binomial 4))
  ;;=> [1 4 6 4 1 0 0 0 0 0]

  ;; (1 + z)^(1/2)
  (into [] g/take-ten (g/binomial (/ 2)))
  ;;=> [1 1/2 -1/8 1/16 -5/128 7/256 -21/1024 33/2048 -429/32768 715/65536]

  ;;=> 1/(1+z)^2
  (into [] g/take-ten (g/binomial (/ -2)))
  ;;=> [1 -1/2 3/8 -5/16 35/128 -63/256 231/1024 -429/2048 6435/32768 -12155/65536]

  ;;=> 1/(1-z)
  (into [] g/take-ten (g/binomial -1 -1))
  ;;=> [1 1 1 1 1 1 1 1 1 1]

  ;;=> 1/(1+z)
  (into [] g/take-ten (g/binomial -1))
  ;;=> [1 -1 1 -1 1 -1 1 -1 1 -1]

  ;; (1-z)^(1/2)
  (into [] g/take-ten (g/binomial -1 (/ 2)))
  ;;=> [1 -1/2 -1/8 -1/16 -5/128 -7/256 -21/1024 -33/2048 -429/32768 -715/65536]

  ;;=> 1/(1-z)^2
  (into [] g/take-ten (g/binomial -1 -2))
  ;;=> [1 2 3 4 5 6 7 8 9 10]

  ;;=> 1/(1-z^2)
  ;;=> [1 0 1 0 1 0 1 0]
  (into [] g/take-ten (g/interleave-m-zeros 1 (g/geo 1)))
  ;;=> [1 0 1 0 1 0 1 0 1 0]

  ;; 1/(1 - (z^m)) = (g/interleave-m-zeros (dec m) (g/geo 1))
  (into [] g/take-ten (g/comp-zm 4 (g/binomial -1 -1)))
  (into [] g/take-ten (g/comp-zm 4 g/e))
  ;;=> [1 0 0 0 1 0 0 0 1/2 0]

  ;;=> 1/(1-z^m)
  ;;=> [1 0 0 ... 0 1 0 0 ... 0 1 ...]

  (into [] (comp (g/comp-zm 4) g/take-ten) (g/binomial -1 -2))
  ;;=> [1 0 0 0 2 0 0 0 3 0]
  )
