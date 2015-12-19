(ns vector.affine
  "affine transformations of the plane as functions")

(comment
  (require '[vector.affine] :reload)
  (in-ns 'vector.affine)
  )

;; implemented as functions on R2, the plane
(defn translate
  ""
  [[vx vy]]
  (fn [[x y]]
    [(+ x vx) (+ y vy)]))

(defn rotate
  ""
  [theta]
  (let [c (Math/cos theta)
        s (Math/sin theta)]
    (fn [[x y]]
      [(+ (* c x) (* (- s) y))
       (+ (* s x) (* c y))])))

(defn reflect
  "reflection in x-axis"
  []
  (fn [[x y]]
    [x (- y)]))

(defn scale
  ""
  ([s] (scale s s))
  ([sx sy]
   (assert (and (not (zero? sx)) (not (zero? sy))))
   (fn [[x y]]
     [(* sx x) (* sy y)])))

(defn shear-x
  ""
  [s]
  (fn [[x y]]
    [x (+ (* s x) y)]))

(defn shear-y
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
