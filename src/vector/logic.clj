(ns vector.logic
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic])
  (:require
   [clojure.core.logic.fd :as fd]
   [clojure.core.logic.pldb :as pldb]))

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
  (let []))

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
