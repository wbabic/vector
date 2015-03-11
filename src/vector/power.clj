(ns vector.power)

(def average-power-watts [90 30 10 15 5])

(def time-in-use [1.4 1.2 0.6 0.2 2.0])

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

(comment

  (dot (mapv #(* % 1000) average-power-watts)
       time-in-use)

  (/ (p/dot p/average-power-watts
            p/time-in-use)
     1000)
  )
