(ns vector.integer
  "explore the space of integers")

;; the story of numbers in clojure

;; inc dec integer?
;; a generator of integer
;; zero?
;; define one?
;; an infinite lazy sequence
;; starting from 0, 1, or n
;; and going to infinity

(def non-negatives
  (iterate inc 0))

(def naturals
  (iterate inc 1))

(def negatives (map #(- %) naturals))

(defn nats-from-n [n]
  (drop n non-negatives))

(def zero-seq (repeat 0))

(defn counter
  [n]
  "takes any input sequence and returns the number of items received
starting from n"
  (assert (integer? n))
  (fn [rf]
    (let [cn (volatile! (dec n))]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
         (let [m @cn
               mn (vswap! cn inc)]
           (rf result mn)))))))

;; the sequence 1 1/2 1/3 1/4 ... 1/n
(def harmonic
  (map #(/ %) naturals))

(defn add
  "add two integers"
  [p q]
  (assert (and (integer? p) (integer? q)))
  (if (zero? p)
    q
    (add (dec p) (inc q))))

(comment
  (require '[vector.integer] :reload)
  (in-ns 'vector.integer)
  (use 'clojure.repl)

  (take 10 non-negatives)
  ;;=> (0 1 2 3 4 5 6 7 8 9)
  (take 10 naturals)
  ;;=> (1 2 3 4 5 6 7 8 9 10)
  (take 10 negatives)
  ;;=> (-1 -2 -3 -4 -5 -6 -7 -8 -9 -10)
  (take 10 zero-seq)
  ;;=> (0 0 0 0 0 0 0 0 0 0)

  (take 10 (eduction (counter 1) zero-seq))
  ;;=> (1 2 3 4 5 6 7 8 9 10)
  ;; could also do
  (take 10 (iterate inc 1))
  ;;=> (1 2 3 4 5 6 7 8 9 10)

  (take 10 harmonic)
  ;;=> (1 1/2 1/3 1/4 1/5 1/6 1/7 1/8 1/9 1/10)

  (take 10 (reductions + harmonic))

  (add 100000 0)
  ;;=> StackOverflowError   clojure.lang.Numbers$LongOps.inc (Numbers.java:545)
  )

;; from perspective of the number protocols
