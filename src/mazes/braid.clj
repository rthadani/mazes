(ns mazes.braid
  (:require [mazes.grid :refer [neighbors link-cell dead-ends links]]
            [mazes.binary-tree :as binary-tree]
            [mazes.sidewinder :as sidewinder]
            [mazes.aldous-broder :as aldous-broder]
            [mazes.hunt-and-kill :as hunt-and-kill]
            [mazes.recursive-backtracker :as recursive-backtracker]
            [clojure.set :as set]))

(defn- modify-dead-end
  [grid cell p]
  (if (> (rand) p)
    grid
    (let [n (into #{} (keys (neighbors cell grid)))
          l (into #{} (keys (links cell grid)))
          link-to (rand-nth (into [] (set/difference n l)))]
      (link-cell cell link-to grid))))

(defn braid
  [grid p]
  (reduce
    (fn [g c]
      (modify-dead-end g c p))
       grid
       (dead-ends grid)))

(defn maze-generator
  [algo height width & {:keys [p] :or {p 1.0}}]
  (->
    (algo height width)
    (braid p)))

(defn draw-grid
  [algo height width]
  (mazes.grid/draw-grid (maze-generator algo height width)))
