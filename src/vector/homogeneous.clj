(ns vector.homogeneous
  "homogeneous coordinates in the Euclidean Plane"
  (:require [vector.transforms :as t]))

(comment
  (require '[vector.homogeneous] :reload)
  (in-ns 'vector.homogeneous)
  (use 'clojure.repl)
  )


(comment
  (def p [1 2])
  )

(defn homogenize
  "for a given point in Euclidean Plane,
  return its homogeneous coordinates"
  [p]
  (conj p 1))

(defn normalize
  "normalize given homgenous coordinates"
  [p]
  (let [[x y w] p]
    (assert (not (zero? w)) (str "homogeneous coordinates "
                                 p
                                 " do not represent a euclidean point"))
    [(/ x w) (/ y w) 1]))

(defn dehomogenize
  "return the Euclidean point
  associated with given homogeneous coordinates"
  [p]
  (let [[x y _] (normalize p)]
    [x y]))

(defn equivalent?
  "tests if two given homogenous coordinates
  represent the same euclidean point"
  [p1 p2]
  (= (normalize p1) (normalize p2)))

(defn cross
  [[a1 a2] [b1 b2]]
  (- (* a1 b2) (* a2 b1)))

(defn expand
  [[a1 a2 a3] [b1 b2 b3]]
  (let [l1 (cross [a2 a3] [b2 b3])
        l2 (* -1 (cross [a1 a3] [b1 b3]))
        l3 (cross [a1 a2] [b1 b2])]
    [l1 l2 l3]))

;; line between two homgeneous points
(defn line
  "return homgeneous line between two homgeneous points"
  [a b]
  (expand a b))

(defn incident?
  "returns true if point lies on line"
  [point line]
  (= 0 (t/dot point line)))

(comment
  ;; property: line between two points contains those two points
  (let [p1 (gen-homog-point)
        p2 (gen-homg-point)
        l (line p1 p2)]
    (and (incident? p1 l)
         (incident? p2 l)))
  )

;; intersection of two lines
(defn intersection
  "return the intersection of two homogeneous lines l1 and l2"
  [l m]
  (expand l m))
