(ns vector.systems.euler
  (:require [clojure.core.matrix :as m]
            [clojure.core.matrix.operators :as o]
            [clojure.core.async :refer [go go-loop chan <! >!]]
            [clojure.core.match :refer [match]]))

;; euler's methods for 2d systems
;; diff eq's are now vector fields

(defn next-step
  "point + step * vector"
  [point step vector-field]
  (o/+ point (o/* step (vector-field point))))

(defn euler-loop
  ([config] (euler-loop congif (chan)))
  ([config out]
   (let [{:keys [step-size initial-condition tf diff-eq]}]
     (go
       (>! out [:start [0 ]])))))

(defn distance
  [v w]
  (m/distance v w))

(def ex1
  {:step-size 0.25
   :number-of-steps 5
   :diff-eq (fn [[x y]] [y (+ (* -2 x) (* -3 y))])
   :initial-condition [1 1]})

(def ex2
  {:diff-eq (fn [[x y]] [-y x])
   :solution (fn [t] [(Math/cos t) (Math/sin t)])
   :iitnial-condition [1 0]
   :ti 0
   :tf 10
   :step-size 0.5})

(comment
  (require '[vector.systems.euler :as e] :reload)
  (def F (:diff-eq e/ex1) )
  (def l (fn [p] (e/next-step p 0.25 F)))
  (take 5 (iterate l [1 1]))
  )
