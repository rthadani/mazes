(ns mazes.utils)

(defn empty-rand-nth
  [coll]
  (if (empty? coll)
    nil
    (rand-nth coll)))
