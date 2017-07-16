(ns mazes.binary-tree
  (:require [mazes.grid :refer [grid generate-cords link-cell neighbors draw-grid]]
            [mazes.utils :as utils]))



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
              (utils/empty-rand-nth)
              (first)
              (lc grid))
            grid)))                                         ; no neighbors for the last cell
    (grid height width)
    (generate-cords height width)))

(defn draw-binary-tree-grid
  [height width]
  (draw-grid (binary-tree height width)))