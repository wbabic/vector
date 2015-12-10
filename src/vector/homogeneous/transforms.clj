(ns vector.homogeneous.transforms
  "matrix representations of affine transforms
  using homgeneous coordinates"
  (:require
   [vector.homogeneous :as h]
   [vector.transforms :as t]
   [clojure.core.matrix :as m]
   [clojure.core.matrix.operators :as o]))

(defn translation
  "matrix corresponding to translation by a 2d vector v"
  [[vx vy]]
  [[1 0 0] [0 1 0] [vx vy 1]])

(defn scale
  "return non umiform scaling matrix for scaling factors sx sy"
  ([s] (scale s s))
  ([sx sy]
   (t/diagonal-matrix [sx sy 1])))

(defn rotation
  "matrix of rotation by theta about z-axis counter clockwise"
  [theta]
  (let [c (Math/cos theta)
        s (Math/sin theta)]
    [[c (- s) 0]
     [s c 0]
     [0 0 1]]))

(defn compose
  "compose two matrices"
  [t1 t2]
  (o/* t1 t2))

(comment
  (require '[vector.homogeneous.transforms] :reload)
  (in-ns 'vector.homogeneous.transforms)
  (use 'clojure.repl)
  )
