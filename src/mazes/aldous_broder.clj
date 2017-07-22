(ns mazes.aldous-broder
  (:require [mazes.grid :refer [grid neighbors links link-cell generate-cords]]))

(defn update-link
  [grid cell [dir neighbor]]
  (let [new-grid (link-cell cell dir grid)]
    [new-grid (get-in new-grid (:location neighbor))]))

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
      (empty? (links (second neighbor) grid))
      (let [[new-grid next-cell] (update-link grid cell neighbor)]
        (recur (dec unvisited) new-grid next-cell (random-neighbor next-cell new-grid)))
      :else
      (recur unvisited grid (second neighbor) (random-neighbor (second neighbor) grid)))))

(defn draw-grid
  [height width]
  (mazes.grid/draw-grid (maze-generator height width)))
