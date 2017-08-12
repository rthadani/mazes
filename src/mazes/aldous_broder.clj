(ns mazes.aldous-broder
  (:require [mazes.grid :refer [grid neighbors links link-cell generate-cords has-links? update-link]]
            [mazes.utils :as utils]))

(defn random-neighbor
  [cell grid]
  (rand-nth (into [] (neighbors cell grid))))

(defn maze-generator
  [height width]
  (loop [unvisited (dec (* height width))
         grid (grid height width)
         cell (get-in grid (rand-nth (generate-cords height width)))
         neighbor (random-neighbor cell grid)]
    (cond
      (<= unvisited 0) grid
      (not (has-links? (second neighbor)))
      (let [[new-grid next-cell] (update-link grid cell neighbor)]
        (recur (dec unvisited) new-grid next-cell (random-neighbor next-cell new-grid)))
      :else
      (recur unvisited grid (second neighbor) (random-neighbor (second neighbor) grid)))))

(defn draw-grid
  [height width]
  (mazes.grid/draw-grid (maze-generator height width)))
