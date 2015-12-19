(ns vector.affine.transforms2d
  "2d affine transforms in matrix form"
  (:refer-clojure :exclude [vector vector? apply])
  (:require
   [clojure.core.matrix :as m]
   [clojure.core.matrix.operators :as o]))

  (comment
  (require '[vector.affine.transforms2d] :reload)
  (in-ns 'vector.affine.transforms2d)
  (use 'clojure.repl))

;; affine coordinates
;; vectors [x y 0]
;; points [x y 1]
;; vectors have direction and length
;; vectors don't translate
;; points have no direction or length

;; position -> point, heading -> vector

(def initial-turtle {:position (point 0 0) :heading (vector 1 0)})

;; a transform can be applied to a vector or point
;; has an inverse and can be composed with other transforms
(defprotocol Transform
  (apply-transform [transform vector-or-point])
  (inverse [transform])
  (compose [transform other-transform]))

(defn conjugate
  "t2 t1 t2^-1"
  [t1 t2]
  (compose t2 (compose t1 (inverse t2))))

(defn point
  ([x y]
   [x y 1])
  ([[x y]]
   [x y 1]))

(defn vector
  ([x y] [x y 0])
  ([[x y]] [x y 0]))

(defn point?
  [[x y z]]
  (== z 1))

(defn vector?
  [[x y z]]
  (== z 0))

;; conformal transformations as data
(def origin (point [0 0]))
(def zero-vector (vector [0 0]))

(defrecord Matrix [M])

(defn translation
  "return 3 by 3 translation matrix for given vector [vx vy]"
  ([[vx vy]]
   (->Matrix
    [[1 0 0]
     [0 1 0]
     [vx vy 1]]))
  ([[vx vy] d]
   (->Matrix
    [[1 0 0]
     [0 1 0]
     [(* d vx) (* d vy) 1]])))

(defn rotation
  "return matrix of rotation by theta, counter clockwise about origin"
  ([theta]
   (let [c (Math/cos theta)
         s (Math/sin theta)]
     (->Matrix
      [[c s 0]
       [(- s) c 0]
       [0 0 1]])))
  ([[q1 q2] theta]
   (let [c (Math/cos theta)
         s (Math/sin theta)
         e (+ (* q1 (- 1 c)) (* q2 s))
         f (- (* q2 (- 1 c)) (* q1 s))]
     (->Matrix
      [[c s 0]
       [(- s) c 0]
       [e f 1]]))))

(defn scale
  "return uniform scaling matrix with scale factor s"
  ([s]
   (->Matrix
    [[s 0 0]
     [0 s 0]
     [0 0 1]]))
  ([[q1 q2] s]
   (->Matrix
    [[s 0 0]
     [0 s 0]
     [(* (- 1 s) q1) (* (- 1 s) q2) 1]])))

(defn general
  "return general affine taransform for given u v Q"
  [u v Q]
  (->Matrix [u v Q]))

(extend-protocol Transform
  Matrix
  (apply-transform [{M :M} w] (m/mmul w M))
  (inverse [{M :M}] (->Matrix (m/inverse M)))
  (compose [{T :M} {S :M}] (->Matrix (o/* T S))))

;; the matrix of the transformation
;; that takes the standard frame (zero, e1, e2)
;; to any given frame (Q u v)
;; a general affine transformation

(comment
  (apply-transform (translation [3 4]) (point 1 0))
  ;;=> [4 4 1]
  (apply-transform (translation [3 4]) (vector 1 0))
  ;;=> [1 0 0]

  (apply-transform (rotation (/ Math/PI 2)) [1 0 1])
  ;;=> [6.123233995736766E-17 1.0 1]

  (let [origin [0 0 1]
        e1 [1 0 0]
        e2 [0 1 0]
        u [2 0 0]
        v [1 1 0]
        Q [1 1 1]
        M [u v Q]
        M-inv (m/inverse M)]
    (assert (and
             (= u (m/mmul e1 M))
             (= v (m/mmul e2 M))
             (= Q (m/mmul origin M))
             (= [1.0 0.0 0.0] (m/mmul u M-inv)))))

  (let [u (vector 2 0)
        v (vector 1 1)
        Q (point 1 1)
        M (general u v Q)
        e1 (vector 1 0)
        e2 (vector 0 1)
        O (point 0 0)]
    (= [u v Q] (mapv #(apply-transform M %) [e1 e2 O])))

  (let [u (vector 2 0)
        v (vector 1 1)
        Q (point 1 1)
        M (general u v Q)
        N (inverse M)
        e1 (vector 1 0)
        e2 (vector 0 1)
        O (point 0 0)]
    (= [1.0 0.0 0.0] (apply-transform N u))
    (= [0.0 1.0 0.0] (apply-transform N v))
    (= [0.0 0.0 1.0] (apply-transform N Q)))
  ;;=> true
  )

;; reuduce turtle
(defn transform-turtle
  [transform turtle]
  (let [f #(apply-transform transform %)]
    (-> turtle
        (update-in [:position] f)
        (update-in [:heading] f))))

(comment
  (transform-turtle
   (translation [2 3])
   initial-turtle)

  (transform-turtle
   (rotation (/ Math/PI 2))
   initial-turtle)
  )
