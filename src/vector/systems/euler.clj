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
  ([config] (euler-loop config (chan)))
  ([config out]
   (let [{:keys [step-size initial-condition ti tf diff-eq]} config
         dt step-size
         next (fn [p] (next-step p step-size diff-eq))]
     (go
       (>! out [:start])
       (>! out [:step 0 ti initial-condition])
       (loop [k 1 t (+ ti dt) p initial-condition]
         (when (<= t tf)
           (let [pn (next p)]
             (>! out [:step k t pn])
             (recur (inc k) (+ t dt) pn))))
       (>! out [:end])))
   out))

(defn f
  [x]
  (format "%7.3f"  (double x)))

(defn print-point
  [[x y]]
  (println
   "x: " (f x) "y: "(f y)))

(defn print-step
  "format and print a vector"
  [k t x y]
  (println
   (format "%3d" k)
   (format "%5.2f"  (double t))
   (f x) (f y)))

(defn print-results
  [results]
  (go-loop []
    (match (<! results)
           [:start]
           (do
             (println "")
             (recur))
           [:step k t [x y]]
           (do
             (print-step k t x y)
             (recur))
           [:end] (println "end"))))

(defn distance
  [v w]
  (m/distance v w))

(defn compare-approximation
  [config results]
  (let [solution (:solution config)
        check-points (:check-points config)
        _ (println check-points)]
    (go-loop []
      (match (<! results)
             [:start] (do
                        (println "")
                        (recur))
             [:step k t [x y]]
             (do
               (print-step k t x y)
               (when (check-points t)
                 (println "checkpoint")
                 (let [s (solution t)
                       d (distance s [x y])
                       l1 (distance s [0 0])
                       l2 (distance [x y] [0 0])]
                   (print "solution: ")
                   (print-point s)
                   (println "distance: " (f d))
                   (println "l1: " (f l1))

                   (println "l2: " (f l2))))
               (recur))
             [:end] (println "end")))))

(def ex1
  {:step-size 0.25
   :ti 0
   :tf (* 0.25 5)
   :diff-eq (fn [[x y]] [y (+ (* -2 x) (* -3 y))])
   :initial-condition [1 1]})

(def ex2
  {:diff-eq (fn [[x y]] [(- y) x])
   :solution (fn [t] [(Math/cos t) (Math/sin t)])
   :check-points #{4 6 10}
   :initial-condition [1 0]
   :ti 0
   :tf 10
   :step-size (/ 2)})

(def ex2b (assoc ex2 :step-size (/ 10)))

(def hw1
  {:diff-eq (fn [[x y]] [y (- (Math/sin x))])
   :initial-condition [0 2]
   :step-size (/ 4)
   :ti 0
   :tf (* (/ 4) 8)})

(def hw2
  {:diff-eq (fn [[x y]]
              [(+ y (* y y))
               (+ (- x) (/ y 5) (* -1 x y) (* 6/5 y y))])
   :initial-condition [1 1]
   :step-size (/ 4)
   :ti 0
   :tf (* (/ 4) 5)})

(def hw3
  {:diff-eq (fn [[x y]] [(* 2 x) y])
   :initial-condition [1 3]
   :step-size (/ 2)
   :solution (fn [t] [(Math/exp (* 2 t)) (* 3 (Math/exp t))])
   :check-points #{2 4 6}
   :ti 0
   :tf 6})

(def hw3b (assoc hw3 :step-size (/ 10)))

(def hw4
  {:diff-eq (fn [[y v]] [v (+ (* -1/2 v) (* -4 y))])
   :initial-condition [2 0]
   :step-size (/ 4)
   :ti 0
   :tf 10})

(defn to-vec
  [config]
  (let [{:keys [ti tf step-size diff-eq initial-condition]} config
        num-steps (/ (- tf ti) step-size)
        next (fn [p] (next-step p step-size diff-eq))]
    (into [] (take num-steps (iterate next initial-condition)))))

(def hw4b (assoc hw4 :step-size (/ 10)))

(comment
  (require '[vector.systems.euler :as e] :reload)
  (def F (:diff-eq e/ex1) )
  (def l (fn [p] (e/next-step p 0.25 F)))
  (take 5 (iterate l [1 1]))

  (e/print-results (e/euler-loop e/ex1))
  (e/print-results (e/euler-loop e/ex2))
  (e/compare-approximation e/ex2 (e/euler-loop e/ex2))
  (e/compare-approximation e/ex2b (e/euler-loop e/ex2b))
  (e/print-results (e/euler-loop e/hw1))
  (e/print-results (e/euler-loop e/hw2))
  (e/print-results (e/euler-loop e/hw3))
  (e/print-results (e/euler-loop e/hw3b))
  (e/compare-approximation e/hw3 (e/euler-loop e/hw3))
  (e/compare-approximation e/hw3b (e/euler-loop e/hw3b))
  (e/print-results (e/euler-loop e/hw4))

  (spit "hw4.clj" (pr-str (e/to-vec e/hw4)))
  (read-string (slurp "hw4.clj"))
  )
