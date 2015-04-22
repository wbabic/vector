(ns vector.pascal
  (:require [clojure.math.numeric-tower :as math]))

;; combinations C(n,k)
;; as an infinate lazy sequence

(defn next-row
  "calculate next row based on prervious row"
  [row]
  (map +
       (conj row 0)
       (concat row '(0))))

(defn combinations [] (iterate next-row '(1)))

(comment
  (require '[vector.pascal :as p] :reload)
  ;; first 10
  (take 10 (p/combinations))
  ;; tenth
  (nth (p/combinations) 10)

  )


(defn pr-pascal
  "print first n rows of Pascal's triangle"
  [n]
  (doseq [row (take n (combinations))]
    (doseq [d row]
      (print d " "))
    (println)))

(defn mid
  "midway point of row n"
  [n]
  (math/ceil (/ (inc n) 2)))

(defn combos
  "first m combinations as a vector of vectors
  up to and including mip point of each row"
  [m]
  (vec
   (for [n (range m)]
     (vec
      (let [t (mid n)]
        (take t (nth (combinations) n)))))))

(def combos-50
  (combos 50))

;; all combinations less than 100
(defn C [n k]
  (let [c combos-50
        m (mid n)]
    ;; n < 0 or k > n returns 0
    ;; n > 100 returns error
    (if (< n m)
      (get-in c [n k])
      (get-in c [n (- n k)]))))

(defn catalan [n]
  (/ (C (* 2 n) n) (inc n)))

(comment
  (p/C 6 3)
  (p/catalan 10)
  (map p/catalan (range 10))
)
