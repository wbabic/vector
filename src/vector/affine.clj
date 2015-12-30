(ns vector.affine
  "affine transformations of the plane as functions")

(comment
  (require '[vector.affine] :reload)
  (in-ns 'vector.affine)
  )

;; implemented as functions on R2, the plane
(defn translate
  "returns function that translates by vector v"
  [[vx vy]]
  (fn [[x y]]
    [(+ x vx) (+ y vy)]))

(defn rotate
  "returns function that rotates by theta about origin counter clockwise"
  [theta]
  (let [c (Math/cos theta)
        s (Math/sin theta)]
    (fn [[x y]]
      [(+ (* c x) (* (- s) y))
       (+ (* s x) (* c y))])))

(defn reflect
  "returns function of reflection in x-axis"
  []
  (fn [[x y]]
    [x (- y)]))

(defn scale
  "returns scale function"
  ([s] (scale s s))
  ([sx sy]
   (assert (and (not (zero? sx)) (not (zero? sy))))
   (fn [[x y]]
     [(* sx x) (* sy y)])))

(defn shear-x
  "shear function along x-axis"
  [s]
  (fn [[x y]]
    [x (+ (* s x) y)]))

(defn shear-y
  "shear function along y-axis"
  [s]
  (fn [[x y]]
    [(+ (* s y) x) y]))

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

(comment
  ((rotate (/ Math/PI 3)) [1 0])
  ;;=> [0.5000000000000001 0.8660254037844386]
  ((rotate (/ Math/PI 3)) [0 1])
  ;;=> [-0.8660254037844386 0.5000000000000001]
  )
