(ns mazes.grid
  (:require [clojure.string :as str]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defprotocol CellOperation
  (link-cell [_ direction grid])
  (neighbors [_ grid])
  (links [_ grid])
  (set-value [_ value grid]))

(def direction-complement
  {:n :s
   :s :n
   :e :w
   :w :e})


(defrecord Cell [location links value]
  CellOperation
  (link-cell [this direction grid]
    (let [n (map :location (map second (neighbors this grid)))
          [lat lng] location
          [ns ew] (condp = direction
                    :n [(dec lat) lng]
                    :s [(inc lat) lng]
                    :e [lat (inc lng)]
                    :w [lat (dec lng)])
          linked (some #{[ns ew]} n)
          linked-cell (when linked (get-in grid linked))]
      (if linked-cell
        (-> (assoc-in grid [lat lng] (->Cell location (conj links direction) value))
            (assoc-in linked (->Cell linked (conj (:links linked-cell) (direction-complement direction)) (:value linked-cell))))
        grid)))

  (neighbors [_ grid]
    (into {}
          (for [offset {:n [-1 0] :s [1 0] :w [0 -1] :e [0 1]}
                :let [[ns ew] (mapv + location (second offset))]
                :when (and (<= 0 ew (dec (count (grid 0)))) (<= 0 ns (dec (count grid))))]
            [(first offset) (get-in grid [ns ew])])))

  (links [_ grid]
    links)

  (set-value [_ value grid]
    (assoc-in grid location (->Cell location links value))))

(defn grid
  [height width]
  (into []
        (for [i (range 0 height)]
          (mapv
            #(->Cell [i %] #{} nil) (range 0 width)))))

(defn draw-grid
  [grid]
  (let [top "---+"
        side "|"
        empty "  "
        corner "+"]
    (doseq [i (range 0 (count grid))
        k (range 0 2)
        j (range 0 (count (grid 0)))
            :let [c (get-in grid [i j])
                  links (links c grid)]]
      ;;k incicates if we are drawing a wall or a floor
      (if (zero? k)
        (when (zero? j)
          (print corner))
        (when (zero? j)
          (print side)))

      (if (zero? k)
        (if-not (contains? links :n)
          (print top)
          (print empty "+"))
        (if-not (contains? links :e)
          (print empty side)
          (print empty " ")))
      (when (= j (dec (count (grid 0))))
        (println "")))
    (print corner)
    (print (str/join "" (repeat (count (grid 0)) top))))
  (println ""))

(defn generate-cords
  [height width]
  (for [i (range 0 height)
        j (range 0 width)]
    [i j]))


