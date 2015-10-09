(ns vector.euclid)

;; euclid's algorithm
(defn gcd [m n]
  (loop [m m n n]
    (let [r (rem m n)]
      (if (zero? r)
        n
        (recur n r)))))

(comment
  (require '[vector.euclid :as euc] :reload)
  (euc/gcd 2166 6099)
  (euc/gcd 6099 2166)
  )

;; extended euclid's algorithm
(defn euclid [m n]
  (loop [a 0 a1 1 b 1 b1 0 c m d n]
    (let [r (rem c d)
          q (quot c d)]
      (println c d q r)
      (if (zero? r)
        (do
          (assert (= d (+ (* a m) (* b n))))
          [a b d])
        (do
          (assert (= c (+ (* a1 m) (* b1 n))))
          (recur (- a1 (* q a)) a (- b1 (* q b)) b d r))))))

(comment
  (euc/euclid 1769 551)
  )

(comment
  (require '[clojure.math.combinatorics :as combo])
  (combo/permutations [1 2 3])
  (combo/count-permutations [1 2 3])
  (combo/permutations [1 1 2])
  (combo/count-permutations [1 1 2])
  (combo/combinations [1 2 3] 2)
  (combo/combinations [1 1 1 2 2] 3)
  (combo/count-combinations [1 1 1 2 2] 3)
  )

(comment
  (require '[clojure.math.numeric-tower :as math])

  (defn- sqr
    "Uses the numeric tower expt to square a number"
    [x]
    (math/expt x 2))

  (defn euclidean-squared-distance
    "Computes the Euclidean squared distance between two sequences"
    [a b]
    (reduce + (map (comp sqr -) a b)))

  (defn euclidean-distance
    "Computes the Euclidean distance between two sequences"
    [a b]
    (math/sqrt (euclidean-squared-distance a b)))

  (let [a [1 2 3 5 8 13 21]
        b [0 2 4 6 8 10 12]]
    (euclidean-distance a b))
  )
