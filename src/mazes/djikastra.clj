(ns mazes.djikastra
  (:require [mazes.grid :refer [set-value links draw-grid]]
            [mazes.binary-tree :refer [binary-tree]]
            [mazes.sidewinder :refer [sidewinder]])
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

;;(draw-grid-distances sidewinder 8 8 [0 0])