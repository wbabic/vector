(ns vector.affine.transforms3d
  "3d affine transformations impemented
  using homogeneous coordinates and matrices
  and thing-geom"
  (:refer-clojure :exclude [vector vector? apply])
  (:require
   [clojure.core.matrix :as cmat]
   [clojure.core.matrix.operators :as o]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.circle :as c]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.color.core :as col]
   [thi.ng.geom.core.matrix :as mat]
   [thi.ng.math.core :as m]))

(comment
  (require '[vector.affine.transforms3d] :reload)
  (in-ns 'vector.affine.transforms3d)
  (use 'clojure.repl)
  )

(comment
  (-> (mat/matrix44) (g/rotate-x m/HALF_PI))

  (-> (mat/matrix44)
      (g/rotate-x m/HALF_PI)
      (g/rotate-z m/SIXTH_PI))
  )
