(ns vector.rat-root-test
  (:require [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test :refer :all]
            [vector.number :as num]
            [vector.root :refer :all]))

(comment
  (require '[vector.rat-root-test] :reload)
  (in-ns 'vector.rat-root-test)
  (use 'clojure.repl)
  (run-tests)
  )

;; first, some specific tests with Phi and phi
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

;; next, general tests using generators and properties

;; generators
;; root generator
(def gen-root
  (gen/fmap (partial apply root)
            (gen/tuple gen/s-pos-int gen/ratio)))

(def gen-nonzero-root
  (gen/such-that #(not (num/zero? %)) gen-root))

(def root-prop
  (prop/for-all [r gen-root]
                (instance? vector.root.Root r)))

;; make sure generators are generating Root's
(defspec gen-root-is-root gen-root root-prop)

(def additive-inverse-root-prop
  (prop/for-all [r gen-root]
                (num/zero? (num/add r (num/negative r)))))

(defspec additive-inverse-root additive-inverse-root-prop)

(def reciprocal-root-prop
  (prop/for-all [r gen-nonzero-root]
                (let [recip (num/multiply r (num/reciprocal r))]
                  (or (== 1 recip)
                      (num/one? recip)))))

(defspec reciprocal-root reciprocal-root-prop)
