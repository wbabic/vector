(ns vector.knapsack
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split]]))

;; read in data file from resources/data

(def test-file-name "resources/data/test1.txt")

(defn parse-line [line]
  (mapv #(Integer/parseInt %) (split line #"\s")))

(defn process-line [index line]
  (let [[v w] (parse-line line)]
    {:value v
     :weight w
     :value-density (float (/ v w))
     :index index}))

(defn parse-file
  "parse knapsack input file and return data structure"
  [file-name]
  (with-open [rdr (io/reader file-name)]
    (let [lines (line-seq rdr)
          [n k] (parse-line (first lines))
          data (vec (map-indexed process-line (rest lines)))]
      {:number-of-items n
       :capacity k
       :items data})))

(defn process-item
  "process one item
returning new weight and updated selections if item is selected
or old ones if not"
  [item total-weight capacity selections]
  (let [new-weight (+ (:weight item) total-weight)]
    (if (< new-weight capacity)
      [new-weight (conj selections (:index item))]
      [total-weight selections])))

(defn fill-greedy
  "greedy algorithm using value density sorting"
  [items-map]
  (let [{:keys [number-of-items capacity items]} items-map
        _ (println "capacity: " capacity)
        sorted-items (sort-by :value-density > items)]
    (loop [total-weight 0
           selections []
           remaining-items sorted-items]
      (let [next-item (first remaining-items)
            _ (println "item: " next-item)]
        (if (or (nil? next-item)
                (= total-weight capacity))
          {:weight total-weight
           :selections selections}
          (let [{:keys [weight value index]} next-item
                [new-weight new-selections]
                (process-item next-item
                              total-weight
                              capacity
                              selections)]
            (recur new-weight new-selections (rest remaining-items))))))))

(comment
  (require '[vector.knapsack :as k] :reload)
  (def f (k/parse-file k/test-file-name))
  (pprint (sort-by :value-density > (:items f)))
  (k/fill-greedy f)
  )
