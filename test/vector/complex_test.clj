(ns vector.complex-test
    (:require [clojure.test.check :as tc]
              [clojure.test.check.generators :as gen]
              [clojure.test.check.properties :as prop]
              [clojure.test.check.clojure-test :refer [defspec]]
              [clojure.test :refer :all]
              [vector.number :as num]
              [vector.complex :as c]))

(comment
  (require '[vector.complex-test] :reload)
  (in-ns 'vector.complex-test)
  (use 'clojure.repl)
  (run-tests)
  )

(deftest reciprocal-omega
  (testing "omega reciprocal times omega is one"
    (let [omega-recip (num/reciprocal c/omega)]
      (is (num/one? (num/multiply c/omega omega-recip))))))

(deftest conjugate-omega
  (testing "omega reciprocal is conjugate omega"
    (is (num/equals? (num/reciprocal c/omega) (num/conjugate c/omega)))))

;; generator for complex numbers with rational entries
(def gen-rational-complex
  (gen/fmap (partial apply c/complex) (gen/tuple gen/ratio gen/ratio)))

(def gen-nonzero-rational-complex
  (gen/such-that #(not (num/zero? %)) gen-rational-complex))

;; let's make sure the generator does indeed generate complex numbers
(defspec gen-complex-prop
  (prop/for-all [c gen-rational-complex]
                (instance? vector.complex.Complex c)))

(defspec additive-inverse-complex-rational
  (prop/for-all [c gen-rational-complex]
                (let [neg-c (num/negative c)]
                  (num/zero? (num/add c neg-c)))))

(defspec reciprocal-complex-rational
  (prop/for-all [c gen-nonzero-rational-complex]
                (let [recip-c (num/reciprocal c)]
                  (num/one? (num/multiply c recip-c)))))

;; now for some more complex numbers
