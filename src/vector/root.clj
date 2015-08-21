(ns vector.root
  "square root of positive integers"
  (:require [vector.protocol :as p]
            [clojure.core.match :refer [match]]))

(comment
  (require '[vector.root] :reload)
  (in-ns 'vector.root)
  )

(def keywords [:number :root])
(def zero 0)

(def rt5 [:root 5])
