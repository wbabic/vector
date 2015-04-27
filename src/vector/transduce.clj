(ns vector.transduce)

;; iterables - to be iterated
;; geometric
(def f1 (fn [[s c]] [(* s c) c]))
(defn geo [c]
  (eduction (map first) (iterate f1 [1 c])))

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
  ([r c]
   (eduction (map first) (iterate (f3 r c) [1 1 1]))))

;; fibonacci
(def f4 (fn [[a b]] [b (+' a b)]))
(def fibs (eduction (map first) (iterate f4 [0 1])))

(comment
  (require '[vector.transduce :as t] :reload)
  (take 10 (iterate t/f1 [1 2]))
  (take 20 (t/geo (/ 2)))
  (take 20 (t/geo 2))

  (take 10 (iterate t/f2 [1 0]))
  (take 10 t/e)

  (take 10 (iterate (t/f3 4) [1 1]))
  (take 5 (t/binomial 5))
  (take 7 (t/binomial (/ -2)))
  (take 7 (t/binomial (/ 2)))

  (take 10 (iterate t/f4 [0 1]))
  (take 20 t/fibs)

  )
