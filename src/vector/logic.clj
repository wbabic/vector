(ns vector.logic
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic])
  (:require
   [clojure.core.logic.fd :as fd]
   [clojure.core.logic.pldb :as pldb]
   [clojure.core.match :refer [match]]
   [clojure.math.numeric-tower :as math]))

;; core.logic experimentation
(comment
  (use 'clojure.core.logic)

  (run* [q] (== q :q))
  (run* [q] (== q [:a :b]))
  ;;=> ([:a :b])
  (run* [q] (== q :q) (== q :x))
  ;;=> ()
  (run* [q] (conde [(== q :q)] [(== q :x)]))
  ;;=> (:q :x)
  (run* [q]
    (conde
     [(== q :q)]
     [(== q :x)])
    (!= q :q))
  ;;=> (:x)
  )

(comment
  (require '[vector.logic] :reload)
  (in-ns 'vector.logic)
  (use 'clojure.repl)
  )

(defn beatso [player1 player2]
  (conde
   [(== player1 :rock) (== player2 :scissors)]
   [(== player1 :scissors) (== player2 :paper)]
   [(== player1 :paper) (== player2 :rock)]))

(comment
  (run* [q] (beatso :rock :paper))
  ;;=> ()
  (run* [q] (beatso :rock :scissors))
  ;;=> (_0)
  (run* [q] (beatso :rock q))
  ;;=> (:scissors)
  (run* [q] (beatso q :rock))
  ;;=> (:paper)

  (run* [q]
    (fresh [x y]
      (beatso x y)
      (== q [x y])))
  ;;=>  ([:rock :scissors] [:scissors :paper] [:paper :rock])
  )

;; facts and relations
(pldb/db-rel man p)
(pldb/db-rel woman p)
(pldb/db-rel likes p1 p2)
(pldb/db-rel fun p)

(def facts
  (pldb/db
   [man 'Bob]
   [man 'John]
   [man 'Ricky]

   [woman 'Mary]
   [woman 'Martha]
   [woman 'Lucy]

   [likes 'Bob 'Mary]
   [likes 'John 'Martha]
   [likes 'Ricky 'Lucy]

   [fun 'Lucy]))

(def facts1 (-> facts (pldb/db-fact fun 'Martha)))

(comment
  (pldb/with-db facts
    (run* [q]
      (fresh [x y]
        (fun y)
        (likes x y)
        (== q [x y]))))
  ;;=> ([Ricky Lucy])

  (pldb/with-db facts1
    (run* [q]
      (fresh [x y]
        (fun y)
        (likes x y)
        (== q [x y]))))
  ;;=> ([John Martha] [Ricky Lucy])

  ;; add indexes
  (pldb/db-rel likes ^:index p1 ^:index p2)
 )


(pldb/db-rel rpslt gesture p)

(pldb/db-rel beats gesture1 gesture2)

(def facts2
  (pldb/db
   [rpslt :rock]
   [rpslt :paper]
   [rpslt :scissors]
   [rpslt :lizard]
   [rpslt :turtle]

   [beats :scissors :paper]
   [beats :paper :rock]
   [beats :rock :lizard]
   [beats :lizard :turtle]
   [beats :turtle :scissors]
   [beats :scissors :lizard]
   [beats :lizard :paper]
   [beats :paper :turtle]
   [beats :turtle :rock]
   [beats :rock :scissors]
   ))

(comment
  (pldb/with-db facts2
    (run* [q]
      (rpslt q)))
  ;;=> (:scissors :paper :lizard :turtle :rock)

  (pldb/with-db facts2
    (run* [q]
      (rpslt q)
      (beats :turtle q)))
  ;;=> (:scissors :rock)

  (pldb/with-db facts2
    (run* [q]
      (rpslt q)
      (beats :lizard q)))
  ;;=> (:paper :turtle)

  (pldb/with-db facts2
   (run* [q]
     (fresh [x y]
       (beats :turtle x)
       (beats x y)
       (beats y :turtle)
       (== q [:turtle x y :turtle]))))
  '([:turtle :rock :lizard :turtle]
    [:turtle :scissors :paper :turtle]
    [:turtle :scissors :lizard :turtle])
  )

;; lets look at time units
(pldb/db-rel equals unit-1 ratio unit-2)

(def units [:year :month :day :hour :minute :second])

(def time-units
  (pldb/db
   [equals :year 12 :month]
   [equals :month 30 :day]
   [equals :day 24 :hour]
   [equals :hour 60 :minute]
   [equals :minute 60 :second]))

(def angle-units
  (pldb/db
   [equals :degree 60 :minute]
   [equals :minute 60 :second]))

(defn degrees->decimal [[degs mins secs]]
  (+ degs (* mins (/ 60)) (* secs (/ 60) (/ 60))))

(defn decimal->degrees [degrees]
  (let [d (int (quot degrees 1))
        r (rem degrees 1)
        mplus (* 60 r)
        m (int (quot mplus 1))
        mr (rem mplus 1)
        splus (* 60 mr)
        s (math/round splus)]
    [d m s]))

(def time-map
  {:year   1
   :month  12
   :day    (* 30 12)
   :hour   (* 24 30 12)
   :minute (* 60 24 30 12)
   :second (* 60 60 24 30 12)})

(defn set-time-map [unit]
  (let [r (unit time-map)]
    (into {} (map (fn [[a b]] [a (/ b r)]) time-map))))

(defn value-of-unit
  [unit]
  (match unit
         :month 12
         :day 30
         :hour 24
         :minute 60
         :second 60
         :year nil))

(def next-unit
  {:second :minute
   :minute :hour
   :hour :day
   :day :month
   :month :year})

(defn reduce-unit
  "reduce unit if value over, returning a sequence
  or just argument if not"
  [[unit amount]]
  (if-let [unit-value (value-of-unit unit)]
    (let [q (quot amount unit-value)
          r (rem amount unit-value)]
      (if (> q 0)
        (list [q (next-unit unit)] [unit r])
        [unit amount]))
    [unit amount]))

(defn convert
  [from to-unit]
  (let [[in-unit amount] from
        m (set-time-map in-unit)]
    [to-unit (* amount (to-unit m))]))

(comment
  (reduce-unit [:year 5000])
  (reduce-unit [:month 14])
  (reduce-unit [:day 35])
  (convert [:day 1] :year)
  (reduce-unit (convert [:day 361] :month))
  )

(comment
  (pldb/with-db time-units
    (run* [q]
      (fresh [x y r1 r2]
        (equals x r1 :day)
        (equals :day r2 y)
        (== q [[:day 1] [y r2] [x [:recip r1]]]))))
  ;;=> ([[:day 1] [:hour 24] [:month [:recip 30]]])

  (pldb/with-db angle-units
    (run* [q]
      (fresh [x r]
        (equals :degree r x)
        (== q [[:degree 1] [x r]]))))
  ;;=> ([[:degree 1] [:minute 60]])

  ;; [degrees minutes seconds]
  ;; [40 20 50] is approximately 40.34722
  ;; 40 degrees 20 minutes 50 seconds
  ;; need symbols for degrees minutes seconds

  ;; questions
  ;; how many seconds in a minute, in a degree?
  ;; how many seconds in a measurement [40 20 50]
  ;; what is a decimal evaluation?
  ;; of minutes, seconds

  ;; given decimal angle, what is its value in degrees minutes and seconds
  ;; 40.34722
  (degrees->decimal [40 20 50])
  ;;=> 40.34722222222222
  )

(defn add-base-60
  [s1 s2]
  (let [s (+ s1 s2)]
    [(quot s 60) (rem s 60)]))

;; lets add two angle measurement
(defn add [a1 a2]
  (let [[d1 m1 s1] a1
        l1 [[:degree d1] [:minute m1] [:second s1]]
        [d2 m2 s2] a2]
    (loop [d d2 m m2 s s2 l l1]
      (if-let [item (first l)]
        (match item
               [:degree d1] (recur (+ d d1) m s (rest l))
               [:minute m1] (let [[q r] (add-base-60 m m1)]
                              (recur (+ d q) r s (rest l)))
               [:second s1] (let [[q r] (add-base-60 s s1)
                                  [q2 r2] (add-base-60 q m)]
                              (recur (+ q2 d) r2 r (rest l))))
        [d m s]))))

(comment
  ;; adding degrees
  (let [m1 [30 3 27]
        m2 [61 24 2]]
    (add m1 m2))
  ;;=> [91 27 29]

  (let [m1 [30 35 27] m2 [61 24 35]] (add m1 m2))
  ;;=> [92 0 2]
  (let [m1 [359 59 59] m2 [0 0 1]] (add m1 m2))
  ;;=> [360 0 0]
  )

;; finite domain
(comment
  (run* [q] (fd/in q (fd/interval 1 9)))
  ;;=> (1 2 3 4 5 6 7 8 9)

  (run* [q]
    (fresh [x y]
      (fd/in x y (fd/interval 1 10))
      (fd/+ x y 10)
      (== q [x y])))
  ;;=> ([1 9] [2 8] [3 7] [4 6] [5 5] [6 4] [7 3] [8 2] [9 1])

  (run* [q]
    (fresh [x y]
      (fd/in x y (fd/interval 0 9))
      (fd/eq
       (= (+ x y) 9)
       (= (+ (* x 2) (* y 4)) 24))
      (== q [x y])))
  ;;=> ([6 3])

  (run* [q]
    (fresh [x y]
      (fd/in x y (fd/interval 1 10))
      (fd/+ x y 10)
      (fd/distinct [x y])
      (== q [x y])))
  ;;=> ([1 9] [2 8] [3 7] [4 6] [6 4] [7 3] [8 2] [9 1])

  )
