(ns vector.population)

;; year -> population in millions
(def stats
  {1790 3.9
   1800 5.3
   1810 7.2
   1820 9.6
   1830 12.9
   1840 17.1
   1850 23.2
   1860 31.4
   1870 38.6
   1880 50.2
   1890 63.0
   1900 76.3
   1910 92.0
   1920 105.7
   1930 122.8
   1940 131.7
   1950 151.3
   1960 179.3
   1970 203.3
   1980 226.5
   1990 248.7
   2000 281.4
   2010 308.7})

;; relative growth
;; at ti
;; 1/pi * (pi+1 - pi-1)/(ti+1 - ti-1)
;; delta p/ delta t over p

(def times (vec (range 1790 2011 10)))

(defn growth-rate [i]
  (let [i1 (dec i)
        i2 (inc i)
        t1 (times i1)
        t2 (times i2)
        _ (println "t1 : " t1)
        _ (println "t2 : " t2)
        dt (- t2 t1)
        p1 (stats t1)
        p2 (stats t2)
        _ (println "p1 : " p1)
        _ (println "p2 : " p2)
        dp (- p2 p1)
        p  (stats (times i))
        _ (println "p : " p)]
    (* (/ p) (/ dp dt))))


(def relative-growth-rates
  (mapv growth-rate (range 1 (dec  (count times)))))
