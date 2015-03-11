(ns vector.core
  (:require [clojure.core.matrix :as m]
            [clojure.core.matrix.operators :as o]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, Walter!"))

(def A [[1 2 3]
        [4 5 6]
        [7 8 9]])
