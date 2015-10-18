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
