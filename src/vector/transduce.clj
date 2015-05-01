(ns vector.transduce)

;; transducer transforming G(z) to G(cz)
(defn z->cz
  "create a statefull transducer that multiplies each entry by
  consecutive powers of c"
  [c]
  (fn [rf]
    (let [cn (volatile! 1)]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
         (let [m @cn
               mn (vswap! cn #(* c %))]
           (rf result (* m input))))))))

;; iterables - to be iterated
;; geometric series using iterate
(def f1 (fn [[s c]] [(* s c) c]))
(defn geo-iter [c]
  (eduction (map first) (iterate f1 [1 c])))

;; using transducers
(defn geo
  ([] (repeat 1))
  ([c] (eduction (z->cz c) (geo))))

;; exponential
(def f2 (fn [[s n]]
          (let [m (inc n)] [(* (/ m) s) m])))
(def e (eduction (map first) (iterate f2 [1 0])))

;; binomial
;; (1+cx)^r
(defn f3
  ([r]
   (fn [[s k]]
     [(/ (* s (- r k)) k) (inc k)]))
  ([r c]
   (fn [[s k cn]]
     [(/ (* cn s (- r k)) k) (inc k) (* c cn)])))
(defn binomial
  ([r]
   (eduction (map first) (iterate (f3 (inc r)) [1 1])))
  ([c r]
   (eduction (map first) (iterate (f3 (inc r) c) [1 1 1]))))

;; fibonacci
(def f4 (fn [[a b]] [b (+' a b)]))
(def fibs (eduction (map first) (iterate f4 [0 1])))

(comment
  (require '[vector.transduce :as t] :reload)
  (take 10 (iterate t/f1 [1 2]))

  (take 20 (t/geo-iter (/ 2)))
  (take 20 (t/geo-iter 2))

  (take 10 (t/geo))
  (take 10 (t/geo (/ 2)))

  (take 10 (iterate t/f2 [1 0]))
  (take 10 t/e)
  ;; e^2z
  (take 10 (eduction (t/z->cz 2) t/e))

  (take 10 (iterate (t/f3 4) [1 1]))
  (take 5 (t/binomial 5))
  (take 7 (t/binomial (/ -2)))
  (take 7 (t/binomial (/ 2)))
  (take 7 (t/binomial (/ -9) (/ 2)))

  (take 10 (iterate t/f4 [0 1]))
  (take 20 t/fibs)

  ;; sequence of partial sums
  (take 10 (reductions + (t/geo (/ 2))))
  ;; (1 3/2 7/4 15/8 31/16 63/32 127/64 255/128 511/256 1023/512)

  (take 10 (t/geo 1))
  (take 10 (sequence (t/z->cz (/ 2)) (t/geo 1)))
  )
