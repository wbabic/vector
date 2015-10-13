(ns vector.root
  "square root of positive integers"
  (:require [vector.number :as num]))

;; constructor functions
(declare root)
(declare rat-roots)
(declare mult-by-root)
(declare mult-by-ratio)

(defrecord Root [base multiplier]
  num/Evaluate
  (num/evaluate [_]
    (* (Math/sqrt base) multiplier))

  num/Equality
  (equals? [x y]
    (if (instance? Root y)
      (and (== base (:base y))
           (== multiplier (:multiplier y)))
      (if (satisfies? num/Equality y)
        (num/equals? y x)
        false)))

  num/Addition
  (num/add [x y]
    (cond
      (number? y) (rat-roots y x)
      (instance? Root y)
      (if (== base (:base y))
        (root base (+ multiplier (:multiplier y)))
        (rat-roots 0 x y))
      :else (num/add y x)))
  (num/negative [_]
    (root base (- multiplier)))
  (num/zero? [_]
    (zero? multiplier))

  num/Conjugate
  (num/conjugate [_]
    (root base (- multiplier)))

  num/Multiplication
  (multiply [x y]
    (mult-by-root x y))
  (one? [_] (and (== 1 base) (== 1 multiplier)))
  (reciprocal [_] (let [denom (* multiplier base)]
                    (if (zero? denom)
                      :infinity
                      (root base (/ denom))))))

;; root constructor
(defn root
  ([base] (root base 1))
  ([base multiplier]
   (->Root base multiplier)))

(def rt5 (root 5))

(def omega (root 3 (/ 2)))

(defn collect-roots
  "collect like bases of given sequence of roots
  ignoring zero multipliers"
  ([] nil)
  ([& roots]
   (let [reduced (reduce
                  (fn [result root]
                    (let [base (:base root)
                          multiplier (:multiplier root)]
                      (update-in result [base]
                                 (fnil (fn [m]
                                         (+ m multiplier))
                                       0))))
                  (sorted-map)
                  roots)]
     (map (fn [[b m]] (root b m))
          (filter (fn [[b m]]
                    (not (zero? m)))
                  reduced)))))

(defn collect-ratios-roots
  "collect like bases of given sequence of roots and ratios
  filtering out non-zero multipliers and returning a RationalRoot"
  ([] (rat-roots 0))
  ([& ratios-and-roots]
   (let [initial {:ratio 0 :roots (sorted-map)}
         reduced (reduce
                  (fn [result ratio-or-root]
                    (cond
                      (or (integer? ratio-or-root) (ratio? ratio-or-root))
                      (update-in result [:ratio] #(+ % ratio-or-root))

                      (instance? Root ratio-or-root)
                      (let [base (:base ratio-or-root)
                            multiplier (:multiplier ratio-or-root)]
                        (if (== 1 base)
                          (update-in result [:ratio] (fnil (fn [n] (+ multiplier n)) 0))
                          (update-in result [:roots base]
                                                   (fnil (fn [m]
                                                           (+ m multiplier))
                                                         0))))))
                  initial
                  ratios-and-roots)]
     (apply rat-roots
            (:ratio reduced)
            (map (fn [[b m]] (root b m))
                 (filter (fn [[b m]]
                           (not (zero? m)))
                         (:roots reduced)))))))

(defn factors [n]
  (reverse
   (sort
    (loop [j 1 res []]
      (if (> (* j j) n) res
          (recur (inc j) (if (zero? (rem n j))
                           (conj res (/ n j) j)
                           res)))))))

(def squares (map #(* % %)))

(defn less-than [n]
  (take-while #(<= % n)))

(defn squares-less-than [k]
  (into #{} (comp squares (less-than k)) (range)))

(defn largest-square-factor [n]
  (let [squares (squares-less-than n)
        factors (factors n)]
    (loop [factors factors]
      (let [f (first factors)]
        (if (squares f) f
            (recur (rest factors)))))))

(defrecord RationalRoot [ratio roots]
  num/Evaluate
  (num/evaluate [_]
    (reduce + ratio (map num/evaluate roots)))

  num/Addition
  (num/add [x y]
    (cond
      (or (integer? y) (ratio? y))
      (apply rat-roots (+ ratio y) roots)

      (instance? Root y)
      (apply rat-roots ratio (apply collect-roots y roots))

      (instance? RationalRoot y)
      (apply rat-roots
             (+ ratio (:ratio y))
             (apply collect-roots (concat roots (:roots y))))))

  (num/negative [_]
    (apply rat-roots (- ratio) (map num/negative roots)))

  (num/zero? [_]
    (and (zero? ratio)
         (every? num/zero? roots)))

  num/Conjugate
  (num/conjugate [_]
    (apply rat-roots ratio (map num/conjugate roots)))

  num/Equality
  (num/equals? [_ y]
    (cond
      (instance? RationalRoot y)
      (and (== ratio (:ratio y))
           (every? true?
                   (map num/equals?
                        (apply collect-roots roots)
                        (apply collect-roots (:roots y)))))))
  num/Multiplication
  (num/multiply [x y]
    ;; assuming y is also a rational root
    (reduce num/add
     (mult-by-ratio ratio y)
     (map #(mult-by-root % y) roots)))

  (num/one? [_]
    (and (== 1 ratio)
         (every? num/zero? roots)))

  (num/reciprocal [_]
    (let [root-count (count roots)]
      (condp = root-count
          0 (rat-roots (/ ratio))
          1 (let [first-root (first roots)
                  b (:base first-root)
                  m (:multiplier first-root)]
              (if (== 0 ratio)
                (num/reciprocal first-root)
                (if (== m 0)
                  (/ ratio)
                  (let [k (/ (- (* ratio ratio) (* m m b)))]
                    (rat-roots (* ratio k) (root b (* (- m) k)))))))))))

(defn rat-roots
  [num & roots]
  (->RationalRoot num roots))

(def rrt5 (rat-roots 0 rt5))

(defn reduce-root [base multiplier]
  (let [lsf (largest-square-factor base)]
    (cond
      (= lsf 1) (root base multiplier)
      (= lsf base) (* (int (Math/sqrt base)) multiplier)
      :else (root (/ base lsf) (* multiplier (int (Math/sqrt lsf)))))))

(defn mult-by-ratio
  "multiply x by a ratio
  x can be a number, a root, or a rat-root"
  [ratio x]
  (cond
    (number? x) (* ratio x)

    (instance? Root x)
    (root (:base x) (* (:multiplier x) ratio))

    (instance? RationalRoot x)
    (apply rat-roots (* ratio (:ratio x))
           (map #(mult-by-ratio ratio %) (:roots x)))))

(defn mult-by-root
  "multiply x by a pure root rt"
  [rt x]
  (assert (instance? Root rt))
  (cond
    (number? x) (mult-by-ratio x rt)

    (instance? Root x)
    (let [b1 (:base rt)
          m1 (:multiplier rt)
          b2 (:base x)
          m2 (:multiplier x)]
      (if (== b1 b2)
        (* b1 m1 m2)
        (reduce-root (* b1 b2) (* m1 m2))))

    (instance? RationalRoot x)
    (reduce num/add
     (mult-by-root rt (:ratio x))
     (map #(mult-by-root rt %) (:roots x)))))

(comment
  (require '[vector.root] :reload)
  (in-ns 'vector.root)

  ;; property: a number plus it's negative is zero
  (def w (rat-roots 1 rt5 omega))
  (def v (rat-roots 1 omega rt5))
  (num/zero? (num/add w (num/negative w)))
  ;;=> true

  ;; adding two roots does not yet have a reciprocal
  (num/add rt5 omega)
  ;;=>
  #vector.root.RationalRoot{:ratio 0,
                            :roots (#vector.root.Root{:base 5, :multiplier 1}
                                    #vector.root.Root{:base 3, :multiplier 1/2})}
  )
