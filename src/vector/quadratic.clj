(ns vector.quadratic)

(defn discriminant [a b c]
  (- (* b b) (* 4 a c)))

(defn quadratic [a b c]
  (fn [x] (+ (* a x x)
             (* b x)
             c)))

;; quadratic equation
(defn quad-roots [a b c]
  (let [d (discriminant a b c)]
    (if (> d 0)
      (let [a2 (* 2 a)
            bneg (- b)
            sqrt-d (Math/sqrt d)
            s1 (/ (- bneg sqrt-d) a2)
            s2 (/ (+ bneg sqrt-d) a2)]
        [s1 s2])
      "negative discriminant")))
