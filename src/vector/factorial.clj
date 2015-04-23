(ns vector.factorial
  (:require [clojure.math.combinatorics :as combo]))

(defn factorial [n] (reduce *' (range 1 (inc n))))

(defn falling-factorial [n k]
  (reduce *' (map #(- n %) (range k))))

(defn choose [n k]
  (/ (falling-factorial n k) (factorial k)))

(comment
  (require '[vector.factorial :as fac] :reload)
  (fac/factorial 0)
  (fac/factorial 11)
  (fac/falling-factorial 5 2)
  (fac/choose -1 3)
  (fac/choose (/ -2) 3)
  )
