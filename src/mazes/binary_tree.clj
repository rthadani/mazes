(ns mazes.binary-tree
  (:require [mazes.grid :refer [grid generate-cords link-cell neighbors draw-grid]]))

(defn empty-rand-nth
  [coll]
  (if (empty? coll)
    nil
    (rand-nth coll)))

(defn binary-tree
  [height width]
  (reduce
    (fn [grid coords]
      (let [c (get-in grid coords)
            lc (partial link-cell c)]
        (or (some->
              (neighbors c grid)
              (select-keys [:s :e])
              (vec)
              (empty-rand-nth)
              (first)
              (lc grid))
            grid)))
    (grid height width)
    (generate-cords height width)))

(defn draw-binary-tree-grid
  [height width]
  (draw-grid (binary-tree height width)))