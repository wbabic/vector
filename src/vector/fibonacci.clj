(ns vector.fibonacci)

(defn fibs
  "return the sequence of Fibonacci numbers"
  []
  (letfn [(fib [[a b]] [b (+' a b)])]
    (->> [0 1]
         (iterate fib)
         (map first))))

(defn square [n] (* n n))

(defn sum-fibs-squared [n]
  (reduce + (take n (map square (fibs)))))

;; represent as an extension of rationals
;; 1/2(1 +- sqrt(5))
;; a + b * sqrt-5 as a vector [a b]

(def one [1 0])
(def rt5 [0 1])

(defn as-decimal [[a b]]
  (+ a (* b (Math/sqrt 5))))

;; add, multiply
;; take nth powers
;; evaluate in a polynomial
(defn add [[a1 b1] [a2 b2]]
  [(+ a1 a2) (+ b1 b2)])

(defn negate [[a1 a2]]
  [(- a1) (- a2)])

(defn minus [a b]
  (add a (negate b)))

(defn mult [[a1 b1] [a2 b2]]
  [(+ (* a1 a2) (* 5 b1 b2))
   (+ (* a1 b2) (* a2 b1))])

(defn invert
  "return 1/a"
  [[a1 b1]]
  (let [f (- (* a1 a1) (* 5 b1 b1))]
    [(/ a1 f) (/ (- b1) f)]))

(defn divide
  "a/b"
  [a b]
  (mult a (invert b)))

(defn pow
  "raise to the nth power"
  [a n]
  (loop [b a n n]
    (cond (zero? n) one
          (= 1 n) b
          :else (recur (mult b a) (dec n)))))

(def alpha [(/ 2) (/ 2)])

(def beta [(/ 2) (/ -2)])

(defn fib
  "the nth fibonacci"
  [n]
  (let [an (pow alpha n)
        bn (pow beta n)
        f (invert rt5)]
    (mult f (minus an bn))))

(comment
  (require '[vector.fibonacci :as f] :reload)

  (take 15 (f/fibs))
  (take 15 (map square (f/fibs)))
  ;;=> (0 1 1 4 9 25 64 169 441 1156 3025 7921 20736 54289 142129)
  (map f/sum-fibs-squared (range 1 11))
  ;;=> (0 1 2 6 15 40 104 273 714 1870)

  ;; now look at the product of two adjacent fibs
  (take 10 (partition 2 1 (f/fibs)))
  (->> (take 10 (partition 2 1(f/fibs)))
       (map vec)
       (map (fn [[f1 f2]] (* f1 f2))))
  ;;=> (0 1 2 6 15 40 104 273 714 1870)
  (f/add f/alpha f/beta)
  ;;=> [1N 0N]
  (f/mult f/alpha f/beta)
  ;;=> [-1N 0N]

  ;; beta = -1/alpha
  (= (f/negate (f/invert f/alpha)) f/beta)

  ;; beta = 1 - alpha
  (= f/beta (f/minus f/one f/alpha))

  ;; solutions to
  ;; x^2 = x + 1
  (let [f (fn [x] (= (f/mult x x) (f/add x f/one)))]
    [(f f/alpha) (f f/beta)])
  ;; beta - alpha = rt5
  (= f/rt5 (f/minus f/alpha f/beta))

  ;; pow
  (= (f/pow f/alpha 2) (f/mult f/alpha f/alpha))
  (= (f/pow f/alpha 3) (f/mult f/alpha (f/mult f/alpha f/alpha)))

  )
