(ns mazes.hunt-and-kill
  (:require [mazes.grid :refer [grid neighbors links link-cell generate-cords has-links? update-link]]
            [mazes.utils :as utils]))

(defn random-unvisited-neighbor
  [cell grid]
  (->>
    (neighbors cell grid)
    (into [])
    (filter #(not (has-links? (second %))))
    (utils/empty-rand-nth)))

;;hunt
(defn unvisited-cell-with-visited-neighbor
  [grid]
  (->>
    (for [i (range 0 (count grid))
          j (range 0 (count (grid 0)))
          :let [cell (get-in grid [i j])
                links (links cell grid)
                visited-neighbors (filter (fn [[dir ncell]] (has-links? ncell)) (neighbors cell grid))]
          :when (and (not (has-links? cell)) (not-empty visited-neighbors))]
      [cell (rand-nth visited-neighbors)])
    first))



(defn maze-generator
  [height width]
  (loop [unvisited (dec (* height width))
         grid (grid height width)
         cell (get-in grid (rand-nth (generate-cords height width)))
         neighbor (random-unvisited-neighbor cell grid)]
    (if (<= unvisited 0)
      grid
      (let [[new-grid next-cell] (update-link grid cell neighbor)
            neighbor (random-unvisited-neighbor next-cell new-grid)
            [next-cell neighbor] (if (not (nil? neighbor)) [next-cell neighbor] (unvisited-cell-with-visited-neighbor new-grid))]
        (recur (dec unvisited) new-grid next-cell neighbor)))))

(defn draw-grid
  [height width]
  (mazes.grid/draw-grid (maze-generator height width)))
