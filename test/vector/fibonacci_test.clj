(ns vector.fibonacci-test
  (:require [clojure.test :refer :all]
            [vector.fibonacci :refer :all]))

(deftest first-ten-fibs
  (testing "first ten fibs"
    (let [ten-fibs [0 1 1 2 3 5 8 13 21 34]]
      (is (= ten-fibs (take 10 (fibs)))))))

(deftest evaluate-phi
  (testing "evaluation of Phi"
   (let [expected-phi (* (/ 2) (+ 1 (Math/sqrt 5)))]
     (is (= expected-phi (evaluate Phi))))))

(comment
  (require '[vector.fibonacci-test] :reload-all)
  (clojure.test/run-tests 'vector.fibonacci-test)
  ;; or
  (in-ns 'vector.fibonacci-test)
  (run-tests)

  )
