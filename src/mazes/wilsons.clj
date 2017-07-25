(ns mazes.wilsons
  (:require [mazes.grid :refer [grid neighbors links link-cell generate-cords neighbor-locations neighbor-direction-by-location]]
            [mazes.utils :as utils]))

(defn erase-loop-or-add-path
  [dest-location path]
  (let [idx (.indexOf path dest-location)]
    (if (= -1 idx)
      (conj path dest-location)
      (subvec path 0 (inc idx)))))

(defn random-neighbor-location
  [grid location]
  (->> (neighbor-locations (get-in grid location) grid)
       (rand-nth)))

(defn make-path
  [unvisited-cells grid]
  (loop
    [cell-loc (rand-nth unvisited-cells)
     path []]
    (if (not-any? #(= cell-loc %) unvisited-cells)
      (conj path cell-loc)
      (recur (random-neighbor-location grid cell-loc) (erase-loop-or-add-path cell-loc path)))))

(defn update-unvisited
  [unvisited path]
  (reduce
    (fn [unvisited loc] (remove #(= % loc) unvisited))
    unvisited
    path))

(defn link-locations
  [cell1 cell2 grid]
  (let [c (get-in grid cell1)
      dir (neighbor-direction-by-location (get-in grid cell1) cell2 grid)]
    (if dir
      (link-cell c dir grid)
      grid)))

(defn link-cells
  [grid path]
  (reduce
    (fn [grid [cell1 cell2]]
      (link-locations cell1 cell2 grid))
    grid
    (partition 2 1 path)))

(defn maze-generator
  [height width]
  (loop [grid (grid height width)
         cell (rand-nth (generate-cords height width))
         unvisited (remove #(= % cell) (generate-cords height width))]
    (if (empty? unvisited)
      grid
      (let [path (make-path unvisited grid)
            unvisited (update-unvisited unvisited path)
            grid (link-cells grid path)
            next-cell (utils/empty-rand-nth unvisited)]
        (recur grid next-cell unvisited)))))

(defn draw-grid
  [height width]
  (mazes.grid/draw-grid (maze-generator height width)))
