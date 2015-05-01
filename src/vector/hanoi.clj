(ns vector.hanoi
  (:use [clojure.string :only [lower-case split-lines join]]
        [loom.graph :only [graph fly-graph]]
        [loom.attr :only [hilite-path add-attrs-to-all]]
        [loom.io :only [view]]))

(def ss (sorted-set))

(defn initial-state
  "n disks stacked on the first peg"
  [n]
  [(into ss (range 1 (inc n))) ss ss])

(defn end-state
  "n disks stacked on the first peg"
  [n]
  [ss ss (into ss (range 1 (inc n)))])

;; tower of hanoi
;; each possible position is a node
;; two nodes are connected if there is a valid move from one to the other
(defn moves
  "Given a state, return valid moves from that state.
  For each topmost disk, see if we can move it to another peg.
  The other peg must either have a bigger disk on top or no disk"
  [state]
  (for [[from-peg disk] (map-indexed (fn [index item] (vector index (first item))) state)
        to-peg (range (count state))
        :when (and disk
                   (not= from-peg to-peg)
                   (or (empty? (state to-peg))
                       (< disk (first (state to-peg)))))]
    (-> state
        (update-in [from-peg] disj disk)
        (update-in [to-peg] conj disk))))

(defn find-path
  "Return a path from start to end with the fewest hops
  moves is a function that returns adjacent nodes"
  [moves start end]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY start)
         preds {start nil}
         count 0]
    (when-let [node (peek queue)]
      (println "node: " node " count: " count)
      (let [nbrs (remove #(contains? preds %) (moves node))]
        (when (not (empty? nbrs)) (println "nbrs: " nbrs))
        (if (some #{end} nbrs)
          (reverse (cons end (take-while identity (iterate preds node))))
          (recur (into (pop queue) nbrs)
                 (reduce #(assoc %1 %2 node) preds nbrs)
                 (inc count)))))))

(defn solve
  "solution for n disks"
  [n]
  (let [start (initial-state n)
        end (end-state n)]
    (find-path moves start end)))

(def fg (fly-graph :successors moves :start (initial-state 3)))

(defn view-graph [g] (view g))

(comment
  (require '[vector.hanoi :as h] :reload)
  (h/initial-state 3)
  ;;=> [#{1 2 3} #{} #{}]
  (h/end-state 3)
  ;;=> [#{} #{} #{1 2 3}]
  (h/moves (h/initial-state 3))
  (h/solve 3)

  (use '[loom.graph :only [graph fly-graph]])
  (def fg (fly-graph :successors h/moves :start (h/initial-state 3)))
  (h/view-graph fg)
  (def fg (fly-graph :successors range :weight (constantly 77)))
  ;; is not working!
  )
