(ns mazes.grid
  (:require [clojure.string :as str]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defprotocol CellOperation
  (link-cell [_ direction grid])
  (neighbors [_ grid])
  (neighbor-locations [_ grid])
  (links [_ grid])
  (has-links? [_])
  (neighbor-direction-by-location [_ location grid])
  (set-value [_ value grid]))

(def direction-complement
  {:n :s
   :s :n
   :e :w
   :w :e})

(defrecord Cell [location linked-cells value]
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
        (-> (assoc-in grid [lat lng] (->Cell location (into #{} (conj linked-cells direction)) value))
            (assoc-in linked (->Cell linked (into #{} (conj (:linked-cells linked-cell) (direction-complement direction))) (:value linked-cell))))
        grid)))

  (neighbors [_ grid]
    (into {}
          (for [offset {:n [-1 0] :s [1 0] :w [0 -1] :e [0 1]}
                :let [[ns ew] (mapv + location (second offset))]
                :when (and (<= 0 ew (dec (count (grid 0)))) (<= 0 ns (dec (count grid))))]
            [(first offset) (get-in grid [ns ew])])))

  (neighbor-locations [_ grid]
    (->>
      (neighbors (get-in grid location) grid)
      (map (comp :location second))))

  (neighbor-direction-by-location [_ neighbor-location grid]
    (->>
      (neighbors (get-in grid location) grid)
      (filter (fn [[dir cell]]  (= (:location cell) neighbor-location)))
      (ffirst)) )

  (links [_ grid]
    (let [[lat lng] location]
      (into {}
            (map #(case %
                    :n [:n (get-in grid [(dec lat) lng])]
                    :s [:s (get-in grid [(inc lat) lng])]
                    :e [:e (get-in grid [lat (inc lng)])]
                    :w [:w (get-in grid [lat (dec lng)])])
                 linked-cells))))

  (has-links? [_]
    (not-empty linked-cells))

  (set-value [_ value grid]
    (assoc-in grid location (->Cell location linked-cells value))))

(defn grid
  [height width]
  (into []
        (for [i (range 0 height)]
          (mapv
            #(->Cell [i %] #{} nil) (range 0 width)))))

(defn max-cell-width
  [grid]
  (->> (flatten grid)
      (reduce
        (fn [m {:keys [value]}] (max m (+ 1 (count (str value)))))
        3)))
(defn formatted-cell-value
  [c empty cell-width trailer]
  (if-not (nil? (:value c))
    (let [value-width (count (str (:value c)))
        empty (apply str (repeat (- cell-width value-width) " "))]
      (str (str (:value c)) empty trailer))
    (str empty trailer)))

(defn draw-grid
  [grid]
  (let [cell-width (max-cell-width grid)
        top (str (apply str (repeat cell-width "-")) "+")
        side "|"
        corner "+"
        empty (apply str (repeat cell-width " "))]
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
          (print (str empty "+")))
        (if-not (contains? links :e)
          (print (formatted-cell-value c empty cell-width side))
          (print (formatted-cell-value c empty cell-width " "))))
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
