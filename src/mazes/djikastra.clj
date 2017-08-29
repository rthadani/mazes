(ns mazes.djikastra
  (:require [mazes.grid :refer [set-value links draw-grid]]
            [mazes.binary-tree :as binary-tree]
            [mazes.sidewinder :as sidewinder]
            [mazes.aldous-broder :as aldous-broder]
            [mazes.wilsons :as wilsons]
            [mazes.hunt-and-kill :as hunt-and-kill]
            [mazes.recursive-backtracker :as recursive-backtracker])

  (:import (clojure.lang PersistentQueue)))

(defn grid-value-setter
  [grid location value]
  (set-value (get-in grid location) value grid))

(defn update-grid-distances [grid links {:keys [value]}]
  (reduce
    (fn [g [_ l]] (grid-value-setter g (:location l) (inc value)))
    grid
    links))

(defn unvisited-links [grid cell]
  (->> (links cell grid)
       (filter #(nil? (:value (second %))))))

(defn distances
  [grid start-location]
  (loop
    [grid (grid-value-setter grid start-location 0)
     q (conj (PersistentQueue/EMPTY) (get-in grid start-location))]
    (if (empty? q)
      grid
      (let [c (peek q)
            links (unvisited-links grid c)
            updated-grid (update-grid-distances grid links c)]
        (recur updated-grid (apply conj (pop q) (map #(get-in updated-grid (:location (second %))) links)))))))

(defn draw-grid-distances
  [algo height width start]
  (draw-grid (distances (algo height width) start)))

(defn get-nearest-link-location
  [grid location]
  (->> (links (get-in grid location) grid)
       (map (fn [[_ {:keys [value location]}]] [value location]))
       (apply min-key first)
        second))

(defn breadcrumbs
  [distance-grid end-location start-location]
  (loop [current (:location (get-in distance-grid end-location))
         breadcrumbs [(:location (get-in distance-grid start-location))]]
    (if (or (nil? current) (= current start-location))
      breadcrumbs
      (recur (get-nearest-link-location distance-grid current) (cons current breadcrumbs)))))

(defn shortest-path-to
  [grid start-location end-location]
  (let [distance-grid (distances grid start-location)
        breadcrumbs (breadcrumbs distance-grid end-location start-location)]
    [distance-grid (reduce
                    (fn [grid location]
                      (set-value (get-in grid location) (:value (get-in distance-grid location)) grid))
                    grid
                    breadcrumbs)]))

(defn draw-shortest-path
  [algo height width start-location end-location]
  (let [ empty-grid (algo height width)
        [distance-grid path-grid] (shortest-path-to  empty-grid start-location end-location)]
    (draw-grid empty-grid)
    (draw-grid distance-grid)
    (draw-grid path-grid)))

;;(draw-grid-distances sidewinder/mazw-generator 8 8 [0 0] [7 7])
;;