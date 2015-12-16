(ns vector.affine
  "affine transformations as functions and matrices")

(comment
  (require '[vector.affine] :reload)
  (in-ns 'vector.affine)
  )

;; as functions
(defn scale
  ""
  ([s] (scale s s))
  ([sx sy]
   (assert (and (not (zero? sx)) (not (zero? sy))))
   (fn [[x y]]
     [(* sx x) (* sy y)])))

(defn shearx
  ""
  [s]
  (fn [[x y]]
    [x (+ (* s x) y)]))

(defn sheary
  ""
  [s]
  (fn [[x y]]
    [(+ (* s y) x) y]))

;; similarities
;; motions

(defn radial
  ""
  [r]
  (assert (> r 0))
  (fn [[x y]]
    [(* r x) (* r y)]))

(comment
  (=
   (comp (scale x y) (scale (/ x) (/ y)))
   identity))
