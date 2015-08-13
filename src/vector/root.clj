(ns vector.root
  (:require [vector.protocol :as p]))

(comment
  (require '[vector.root] :reload)
  (in-ns 'vector.root)
  )

(defrecord root [base mult]
  p/MyNumber
  (add [_ w]
    (let [[b m] w]
      (root. ))))
