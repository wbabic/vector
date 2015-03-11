(ns vector.transforms
  (:require [clojure.core.matrix :as m]
            [clojure.core.matrix.operators :as o]))

(defn dot [v w]
  (reduce + (map * v w)))

(defn scale [alpha v]
  (mapv #(* % alpha) v))

(defn add [v w]
  (mapv + v w))

(defn minus [v]
  (scale -1 v))

(defn len-sq [v]
  (dot v v))

(defn length [v]
  (Math/sqrt (len-sq v)))

(defn deg->radian
  "convert degrees to radians"
  [deg]
  (* deg Math/PI (/ 180)))

(defn rot
  "2 x 2 rotation matrix by angle theta about origin"
  [theta]
  (let [c (Math/cos theta)
        s (Math/sin theta)]
    [[c (- s)]
     [s c]]))

(defn multmv
  "multipy matrix times a vector returnaing a new vector"
  [mat vec]
  (m/mmul mat vec))

(defn multmm
  "matrix matrix multiplication"
  [m1 m2]
  (o/* m1 m2))

;; special linear transformations
;; special matrices

(defn zero-vector
  "a zero vector of length n"
  [n]
  (vec (repeat n 0)))

(defn zero
  "zero matrix with m-rows and n columns"
  [m n]
  (vec (repeat m (zero-vector n))))

(defn delta
  "returns 1 if i = j, 0 otherwise"
  ([i j]
   (if (= i j) 1 0))
  ([i j d]
   (if (= i j) d 0)))

(defn unit-vector
  "ith unit vect of dimension n"
  ([i n]
   (vec (for [j (range n)] (delta i j))))
  ([i n d]
   (vec (for [j (range n)] (delta i j d)))))

(defn identity-matrix
  "n by n identity matrix"
  [n]
  (vec (for [i (range n)] (unit-vector i n))))

(defn diagonal-matrix
  "returns a diagonal matrix with given entries"
  [diagonals]
  (let [n (count diagonals)]
    (vec (for [[i diag] (map-indexed vector diagonals)]
           (unit-vector i n diag)))))

(defn transpose
  "return the transpose of the given matrix"
  [matrix]
  (m/transpose matrix))

(defn scalem
  "scale a matrix"
  [alpha matrix]
  (o/* alpha matrix))
