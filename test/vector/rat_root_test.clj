(ns vector.rat-root-test
  (:require [clojure.test :refer :all]
            [vector.number :as num]
            [vector.root :refer :all]))

(comment
  (require '[vector.rat-root-test] :reload)
  (in-ns 'vector.rat-root-test)
  )

(def Phi (rat-roots (/ 2) (root 5 (/ 2))))
(def phi (rat-roots (/ -2) (root 5 (/ 2))))

(deftest phi-conjugate
  (testing "conjugation of Phi"
    (is (num/equals? (num/conjugate Phi) (num/negative phi)))))

(deftest phi-reciprocal
  (testing "reciprocal of phi"
    (is (num/equals? Phi (num/reciprocal phi)))))

(deftest phi-product
  (testing "product of Phi and phi is one"
    (is (num/one? (num/multiply Phi phi)))))
