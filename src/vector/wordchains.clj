(ns vector.wordchains
  (:use [clojure.string :only [lower-case split-lines join]]
        [loom.graph :only [graph fly-graph]]
        [loom.attr :only [hilite-path add-attrs-to-all]]
        [loom.io :only [view]]))

(def dictionary
  (->> (slurp "/usr/share/dict/words")
       split-lines
       (map lower-case)
       (into #{})))

(def alphabet "abcdefghijklmnopqrstuvwxyz")

(defn edits
  "Returns words that differ by one letter"
  [^String word]
  (->> word
       (map-indexed
        (fn [i c]
          (let [sb (StringBuilder. word)]
            (for [altc alphabet :when (not= altc c)]
              (str (doto sb (.setCharAt i altc)))))))
       (apply concat)
       (filter dictionary)))

(defn find-path
  "Return a path from start to end with the fewest hops
  neighbors is a function that returns adjacent nodes"
  [edits start end]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY start)
         preds {start nil}
         count 0]
    (when-let [node (peek queue)]
      (println "node: " node " count: " count)
      (let [nbrs (remove #(contains? preds %) (edits node))]
        (when (not (empty? nbrs)) (println "nbrs: " nbrs))
        (if (some #{end} nbrs)
          (reverse (cons end (take-while identity (iterate preds node))))
          (recur (into (pop queue) nbrs)
                 (reduce #(assoc %1 %2 node) preds nbrs)
                 (inc count)))))))

(comment
  ;; no neighbors in recent loom
  (require '[vector.wordchains :as w] :reload)
  (first w/dictionary)
  (count w/dictionary)
  ;;=> 234371
  (filter w/dictionary ["cuspidor" "cromulent" "xebec"])
  ;;=> ("cuspidor" "xebec")
  (w/find-path w/edits "cat" "dog")
  ;;=> ("cat" "cot" "dot" "dog")
  )
