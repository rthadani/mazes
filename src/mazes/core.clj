(ns mazes.core
  (:require [clojure.tools.cli :refer [parse-opts]]
[clojure.string :as string]
            [mazes.grid :refer [set-value links draw-grid]]
            [mazes.djikastra :refer [draw-shortest-path]]
            [mazes.aldous-broder :as aldous-broder]
            [mazes.binary-tree :as binary-tree]
            [mazes.braid :as braid]
            [mazes.hunt-and-kill :as hunt-and-kill]
            [mazes.recursive-backtracker :as recursive-backtracker]
            [mazes.sidewinder :as sidewinder]
            [mazes.wilsons :as wilsons])
  (:gen-class))


(def cli-options
  [["-a"
    "--algorithm ALGORITHM"
    "algorithm: one of aldous-broder, binary-tree, hunt-and-kill, recursive-bactracker, sidewinder, wilsons"
    :default "binary-tree"
    :validate [(fn [option] (fn [option] (some #(#{"binary-tree", "sidewinder", "aldous-broder", "hunt-and-kill", "recursive-backtracker", "wilsons", "braid"} %) #{option})))  
               "Must be one of aldous-broder, binary-tree, braid, hunt-and-kill, recursive-bactracker, sidewinder, wilsons"]]
   ["-b" "--braid BRAID_PROBABILITY" "braid dead ends with probability" :default 1.0 :parse-fn #(Float/parseFloat %)]
   ["-p" "--shortest-path" "print the shortest path with djikastra"]
   ["-s" "--size SIZE" :default 8 :parse-fn #(Integer/parseInt %)]     
   ["-h" "--help"]])

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(defn validate-args
  [args]
  (let [{:keys [options _ errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options) {:exit-message summary :ok? true}
      errors {:exit-message (error-msg errors)}
      :else {:options options})))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn -main [& args] 
  (let [{:keys [options exit-message ok?]} (validate-args args) 
      {:keys [algorithm shortest-path size braid]} options]
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (let [algo (case algorithm
                   "binary-tree" binary-tree/maze-generator
                   "sidewinder" sidewinder/maze-generator
                   "aldous-broder" aldous-broder/maze-generator
                   "hunt-and-kill" hunt-and-kill/maze-generator
                   "recursive-bactracker" recursive-backtracker/maze-generator
                   "wilsons" wilsons/maze-generator)
            algo (if (< braid 1) (partial braid/maze-generator algo braid) algo)]
        (println algorithm (if (< braid 1) " braided" ""))          
        (if shortest-path
          (draw-shortest-path algo size size [0 0] [(dec size) (dec size)])
          (draw-grid (algo size size)))))))
