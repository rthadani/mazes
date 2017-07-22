(ns mazes.binary-tree
  (:require [mazes.grid :refer [grid generate-cords link-cell neighbors]]
            [mazes.utils :as utils]))

(defn maze-generator
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

(defn draw-grid
  [height width]
  (mazes.grid/draw-grid (maze-generator height width)))