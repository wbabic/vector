(ns vector.factorial
  (:require [clojure.math.combinatorics :as combo]))

(defn factorial [n] (reduce * (range 1 (inc n))))

(comment
  (require '[vector.factorial :as fac] :reload)
  (fac/factorial 11)

  )
