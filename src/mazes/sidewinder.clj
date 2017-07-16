(ns mazes.sidewinder
  (:require [clojure.core.match :refer [match]]
            [mazes.grid :refer [grid generate-cords link-cell neighbors draw-grid]]
            [mazes.utils :as utils]))



(defn handle-run [grid run cell [dir neighbor-cell]]
  (if (= dir :e)
    (let [new-grid (link-cell cell dir grid)]
      [new-grid (conj run (get-in new-grid (:location cell)))])
    (let [run-cell (utils/empty-rand-nth (conj run cell))]
      [(link-cell run-cell :s grid) []])))

(defn update-maze
  [[grid run] current-cell-coordinates]
  (let [c (get-in grid current-cell-coordinates)
        handle-run (partial handle-run grid run c)]
    (or (some->
          (neighbors c grid)
          (select-keys [:s :e])
          (vec)
          (utils/empty-rand-nth)
          (handle-run))
        [grid run])) )

(defn sidewinder
  [height width]
  (first
    (reduce
      update-maze
      [(grid height width) []]
      (generate-cords height width))))

(defn draw-sidewinder-grid
  [height width]
  (draw-grid (sidewinder height width)))