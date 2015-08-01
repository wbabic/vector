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


(def units [:year :month :day :hour :minute :second])

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


(pldb/db-rel rpsls gesture p)

(pldb/db-rel beats gesture1 gesture2)

(def facts2
  (pldb/db
   [rpsls :rock]
   [rpsls :paper]
   [rpsls :scissors]
   [rpsls :lizard]
   [rpsls :turtle]
   [rpsls :birdie]

   [beats :scissors :lizard]
   [beats :paper :turtle]
   [beats :rock :birdie]
   [beats :lizard :paper]
   [beats :lizard :rock]
   [beats :turtle :rock]
   [beats :turtle :scissors]
   [beats :birdie :paper]
   [beats :birdie :scissors]))

(comment
  (pldb/with-db facts2
    (run* [q]
      (rpsls q)))
  ;;=> (:scissors :paper :lizard :turtle :rock :birdie)
  (pldb/with-db facts2
    (run* [q]
      (rpsls q)
      (beats :birdie q)))
  ;;=> (:scissors :paper)
  )

;; lets look at time units
(pldb/db-rel equals unit-1 ratio unit-2)

(def time-units
  (pldb/db
   [equals :hour   (/ 24) :day]
   [equals :minute (/ 60) :hour]
   [equals :second (/ 60) :minute]

   [equals :day   (/ 30) :month]
   [equals :month (/ 12) :year]
   ))

(def angle-units
  (pldb/db
   [equals :minute [:degree (/ 60)]]
   [equals :degree [:minute 60]]
   [equals :minute [:second 60]]
   [equals :second [:minute (/ 60)]]
   ))

(defn from-unit [unit]
  (pldb/with-db angle-units
    (run* [q]
      (fresh [x r]
        (equals unit x)
        (== q [[unit 1] x])))))

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
    [d m s]
    ))

(comment
  (pldb/with-db time-units
    (run* [q]
      (equals :hour q :day)))
  ;;=> (1/24)

  (pldb/with-db angle-units
    (run* [q]
      (fresh [x r]
        (equals :degree x)
        (== q [[:degree 1] x]))))
  ;;=> ([[:degree 1] [:minute 60]])

  ;; [degrees minutes seconds]
  ;; [40 20 50] = [:degrees 40.34722], approximately
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

(comment
  ;; how does a relate to be
  (relate :hour :day)
  ;;=> ([:equals [:hour 1] [:day (/ 24)]] [:equals [:day 1] [:hour 24]])
  ;; 1 day = 24 hours
  ;; 1 hour = (/ 24) day
  (defn relates
    [a b]
    )
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

(defn add-iter [a1 a2]
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

(defn add-reduce [a1 a2]
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
