(ns vector.geo)


;; geographic coordinates
;; degrees minutes seconds
;; direction N S E W
;; [degrees minutes]
;; [longitude lattitude]


;; convention
;; east positive, west negative
;; north positive south negative

(def displacement-vectors
  [[:heading [102 6] [20 36]]
   [:heading [4 18] [0 -59]]
   [:heading [0 -6] [-2 -30]]
   [:heading [1 48] [-3 -39]]
   [:heading [9 35] [6 21]]
   [:heading [-20 -4] [0 31]]
   [:heading [0 31] [-12 -2]]
   [:heading [-98 8] [-9 13]]])

(comment
  "30° 15' N, 97° 45' W"
  [east-heading west-heading]
  contsructor will convert west and south heading to east and north

  [{:degrees 30
    :minutes 15
    :seconds 0}
   {:degrees 97
    :minutes 45}]

  ;; as-decimal
  ;; 30.25

  )


(defn mod-360
  [n]
  (mod n 360))

(defn mod-60
  [n]
  (mod n 60))

(defn add
  "add displacement vector a location
return new location"
  [vector location]
  )

(defn east
  [heading]
  [:east heading])

(defn west
  [heading]
  [:east (map #(- %) heading )])

(defn north
  [heading]
  [:north heading])

(defn south
  [heading]
  [:north (mapv #(- %)  heading)])

(def city-map
  {:austin [(west 97 45) (north 30 15)]
   :london [(west 0 8) (north 51 30)]
   :pisa [(east 10 21) (north 43 43)]
   :brussels [(east 4 21) (north 50 51)]
   :valencia [(east 0 23) (north 39 28)]
   :darmstadt [(east 8 39) (north 49 52)]
   :zurich [(east 8 33) (north 47 22)]
   :krakow [(east 19 56) (north 50 4)]})
