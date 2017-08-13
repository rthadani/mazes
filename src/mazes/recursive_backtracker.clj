(ns mazes.recursive-backtracker
  (:require [mazes.grid :refer [grid neighbors links link-cell generate-cords has-links? update-link]]))

(defn unvisited-neighbors
  [cell grid]
  (->> (neighbors cell grid)
       (filter #(not (has-links? (second %))))))

(defn update-link-and-stack
  [grid cell unvisited-neighbors stack]
  (let [neighbor (rand-nth (into [] unvisited-neighbors))
        [new-grid next-cell] (update-link grid cell neighbor)]
    [new-grid (vec (cons next-cell stack))]))

(defn intial-grid-and-stack
  [height width]
  (let [grid (grid height width)
      stack (vector (get-in grid (rand-nth (generate-cords height width))))]
    [grid stack]))

(defn maze-generator
  [height width]
  (loop [[grid stack] (intial-grid-and-stack height width)]
    (if (empty? stack)
      grid
      (let [cell (get-in grid (:location (first stack)))
            neighbors (unvisited-neighbors cell grid)]
        (if (empty? neighbors)
          (recur [grid (vec (rest stack))])
          (recur (update-link-and-stack grid cell neighbors stack)))))))

(defn draw-grid
  [height width]
  (mazes.grid/draw-grid (maze-generator height width)))
