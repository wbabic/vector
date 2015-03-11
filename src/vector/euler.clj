(ns vector.euler
  (:require [clojure.core.async :refer [go go-loop chan <! >!]]
            [clojure.core.match :refer [match]]))

;; euler's method applied to initial value first order diff eq

(defn euler-loop
  ([config] (euler-loop config (chan)))
  ([config out]
   (let [{:keys [step-size number-of-steps diff-eq initial-value]} config
         [t0 y0] initial-value
         dt step-size]
     (println "")
     (println "")
     (println "step-size: " step-size)
     (println "number-of-steps: " number-of-steps)
     (go
       (>! out [:start [0 t0 y0]])
       (loop [k 0 t t0 y y0]
         (if (<= k number-of-steps)
           (let [m (diff-eq t y)
                 kn (inc k)
                 tn (+ t dt)
                 yn (+ y (* m dt))]
             (>! out [:step [k t y]])
             (recur kn tn yn))
           (>! out [:end]))))
     out)))

(defn print-results
  [results]
  (go-loop []
    (match (<! results)
           [:start initial-value]
           (do
             (println "start:" initial-value)
             (recur))
           [:step [k t y]]
           (do
             (println "step: " (format "%3d" k)
                      " t = " (format "%5.2f"  (double t))
                      " y = " (format "%5.3f"  (double y)))
             (recur))
           [:end] (println "end"))))

(def hw1
  {:step-size 0.1
   :number-of-steps 40
   :diff-eq (fn [t y] (- (* y y) (* t t)))
   :initial-value [-2 -1]})

(def hw2
  {:step-size 0.5
   :number-of-steps 4
   :diff-eq (fn [t y] (Math/exp (/ 2 y)))
   :initial-value [0 2]})

(def hw3
  {:step-size 0.1
   :number-of-steps 100
   :diff-eq (fn [t y] (- (* y y) (* y y y)))
   :initial-value [0 0.2]})

(def hw4a
  {:step-size 0.1
   :number-of-steps 20
   :diff-eq (fn [t y] (+ (* 2 y y y) (* t t)))
   :initial-value [0 -0.5]})

(def hw4b
  {:step-size -0.1
   :number-of-steps 20
   :diff-eq (fn [t y] (+ (* 2 y y y) (* t t)))
   :initial-value [0 -0.5]})

(defn hw5
  [step-size num-steps]
  {:step-size step-size
   :number-of-steps num-steps
   :diff-eq (fn [t y] (Math/sqrt y))
   :initial-value [0 1]})

(defn hw6
  [step-size num-steps]
  {:step-size step-size
   :number-of-steps num-steps
   :diff-eq (fn [t y] (- 2 y))
   :initial-value [0 1]})

(def hw7
  {:step-size 0.5
   :number-of-steps 10
   :diff-eq (fn [t w] (* (- 3 w) (+ w 1)))
   :initial-value [0 0]})

(def hw8
  (let [m 54
        g 9.8
        k 0.18
        tv (Math/sqrt (* m g (/ k)))
        v-target (* 0.95 tv)]
    {:step-size 0.5
     :number-of-steps 60
     :diff-eq (fn [t v] (- (* g) (* (/ k m) v v))) ;; g - k/m * v^2
     :initial-value [0 0]}))

(defn hw8b
  ""
  [ti tf step-size]
  (let [m 54
        g 9.8
        k 0.18
        tv (Math/sqrt (* m g (/ k)))
        v-target (* 0.95 tv)
        number-of-steps (/ (- tf ti) step-size)]
    {:step-size step-size
     :number-of-steps number-of-steps
     :diff-eq (fn [t v] (- (* g) (* (/ k m) v v))) ;; g - k/m * v^2
     :initial-value [0 0]}))

(def ex1
  {:step-size 0.25
   :number-of-steps 4
   :diff-eq (fn [t y] (- 3 y))
   :initial-value [0 2]})


(comment
  ;; from repl user=>
  (require '[vector.euler :as e] :reload)
  (e/print-results (e/euler-loop e/hw1))
  (e/print-results (e/euler-loop e/hw2))
  (e/print-results (e/euler-loop e/hw3))
  (e/print-results (e/euler-loop e/hw4a))
  (e/print-results (e/euler-loop e/hw4b))
  (e/print-results (e/euler-loop (e/hw5 1.0 4)))
  (e/print-results (e/euler-loop (e/hw5 0.5 8)))
  (e/print-results (e/euler-loop (e/hw5 0.25 16)))
  (e/print-results (e/euler-loop (e/hw6 1.0 4)))
  (e/print-results (e/euler-loop (e/hw6 0.5 8)))
  (e/print-results (e/euler-loop (e/hw6 0.25 16)))
  (e/print-results (e/euler-loop e/hw7))
  (e/print-results (e/euler-loop e/hw8))
  (e/print-results (e/euler-loop (e/hw8b 0 20 0.01)))

  (e/print-results (e/euler-loop e/ex1)))
