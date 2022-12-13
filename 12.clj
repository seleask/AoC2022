(ns aoc2022.12
  (:require [clojure.data.priority-map :as pm]))

(defn read-input
  [s]
  (->> (clojure.string/split-lines s)
       (vec)))

(defn find-chars
  [c rows]
   (for [[i row] (map vector (range) rows)
         [j value] (map vector (range) row)
         :when (= value c)]
     [i j]))

(def find-char (comp first find-chars))

(defn neighbour-cells
  [i-upper j-upper [i j]]
  (let [i-deltas #{[1 0] [-1 0]}
        j-deltas (map (comp vec reverse) i-deltas)
        apply-delta #(mapv + [i j] %)
        in-range? (fn [[x y]]
                    (and
                     (< x i-upper) (< y j-upper)
                     (<= 0 x) (<= 0 y)))]
    (->> (concat i-deltas j-deltas)
         (map apply-delta)
         (filter in-range?))))

(defn height->int
  [c]
  (condp = c
    \S (int \a)
    \E (int \z)
    (int c)))

(defn rel-accessible?
  [flip rows here there]
  (let [[there-height here-height] ((if flip reverse identity)
                                    (map #(height->int (get-in rows %)) [there here]))]
    (<= there-height (inc here-height))))

(def ascending-accessible? (partial rel-accessible? false))
(def descending-accessible? (partial rel-accessible? true))

(defn children
  [rows [i j] accessible?]
  (let [neighbours (neighbour-cells (count rows) (count (first rows)) [i j])]
    (->> neighbours
         (filter (partial accessible? rows [i j])))))

(defn all-positions
  [i-size j-size]
  (for [i (range i-size)
        j (range j-size)]
    [i j]))

(defn get-distance
  [distances pos]
  (get distances pos Integer/MAX_VALUE))

(defn dijkstra
  [distances queue neighbour-fn]
  (if (empty? queue)
    distances
    (let [[node distance] (peek queue)]
      (if (> distance (get-distance distances node))
        (recur distances (pop queue) neighbour-fn)
        (let [neighbours (neighbour-fn node)
              [updated-distances updated-queue]
              (reduce
               (fn [[distances queue] neighbour]
                 (let [d (inc distance)]
                   (if (< d (get-distance distances neighbour))
                     [(assoc distances neighbour d) (assoc queue neighbour d)]
                     [distances queue])))
               [distances (pop queue)]
               neighbours)]
          (recur updated-distances updated-queue neighbour-fn))))))

(defn search
  [rows start-char neighbour-fn]
  (let [start-pos (find-char start-char rows)
        distances (-> (into {} (map vector
                                    (all-positions (count rows) (count (first rows)))
                                    (repeat Integer/MAX_VALUE)))
                      (assoc start-pos 0))]
    (dijkstra distances (pm/priority-map start-pos 0) neighbour-fn)))

(defn solve-puzzle
  [s]
  (let [rows (clojure.string/split-lines s)
        neighbour-fn (fn [pos]
                       (children rows pos ascending-accessible?))
        goal-pos (find-char \E rows)]
    (-> (search rows \S neighbour-fn)
        (get goal-pos))))

(defn solve-puzzle-again
  [s]
  (let [rows (clojure.string/split-lines s)
        neighbour-fn (fn [pos]
                       (children rows pos descending-accessible?))
        goal-positions (concat (find-chars \S rows) (find-chars \a rows))
        distances (search rows \E neighbour-fn)]
    (->> (map (partial get distances) goal-positions)
         (apply min))))
