(ns vector.markov
  (:require [clojure.core.matrix :as m]
            [clojure.core.matrix.operators :as o]))

;; transition matrix P
(def P
  (m/matrix [[0.4 0.3 0.1]
             [0.4 0.3 0.6]
             [0.2 0.4 0.3]]))

;; initial probability vector
(def x0 [[1] [0] [0]])
(def x1 [[0] [1] [0]])
(def x2 [[0] [0] [1]])

(defn iterations
  "return lazy sequence of
  applying P to x0"
  [P x0]
  (iterate (fn [x] (m/mmul P x)) x0))

(comment
  (require '[vector.markov :as markov] :reload)
  (take 7 (markov/iterations markov/P markov/x0))

  (last (take 365 (markov/iterations markov/P markov/x0)))
  (last (take 365 (markov/iterations markov/P markov/x1)))


  (last (take 3 (markov/iterations markov/P markov/x0)))
  (last (take 3 (markov/iterations markov/P markov/x1)))
  (last (take 3 (markov/iterations markov/P markov/x2)))
  )
