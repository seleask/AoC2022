(ns aoc2022.8
  (:require [clojure.set :refer [union]]))

(defn read-input
  [s]
  (let [char->int #(Integer/parseInt (.toString %))]
    (->> s
         (clojure.string/split-lines)
         (map #(map char->int %))
         (map vec)
         (vec))))

(defn visible-row-indices
  [trees]
  (let [maxes (reductions max -1 trees)]
    (->> trees
         (map vector maxes)
         (map vector (range))
         (filter (fn [[_ [k t]]] (> t k)))
         (map first))))

; both ways
(defn all-visible-row-indices
  [row]
  (let [[visible-from-left visible-from-right]
        (vector
         (visible-row-indices row)
         (visible-row-indices (reverse row)))]
    (union
     (set visible-from-left)
     (set (map (partial - (dec (count row))) visible-from-right)))))

(defn indices->coords
  [row-idx indices]
  (set (map #(vector row-idx %) indices)))

(defn flip-coords
  [coords]
  (map (comp vec reverse) coords))

(defn visible-trees
  [rows]
  (let [cols (apply map vector rows)
        row->coords (fn [i row]
                      (->> (all-visible-row-indices row)
                           (indices->coords i)))]
    (union
     (apply union
            (map-indexed row->coords
                         rows))
     (apply union
            (map-indexed (comp flip-coords row->coords)
                         cols)))))

; part 2

; inclusive
(defn trees-up-to-obstruction
  [height trees]
  (let [[lt etc] (split-with (partial > height) trees)]
    (concat lt (take 1 etc))))

(defn visible-range-row-from-tree
  [row tree-idx]
  (let [tree (nth row tree-idx)
        [reverse-lhs rhs-and-tree] (split-at tree-idx row)
        rhs (rest rhs-and-tree)
        lhs (reverse reverse-lhs)]

    (->> (map (partial trees-up-to-obstruction tree) [lhs rhs])
         (map count))))

(defn visible-ranges-for-row
  [row]
  (map (partial visible-range-row-from-tree row) (range (count row))))

(defn visible-ranges-for-rows
  [rows]
  (map visible-ranges-for-row rows))

(defn visible-ranges-for-cols
  [rows]
  (let [transpose (partial apply map vector)]
    (->> (transpose rows)
         (visible-ranges-for-rows)
         (transpose))))

(defn combine-rows
  [f xs ys]
  (map f xs ys))

(defn visible-ranges-combined
  [rows]
  (map (partial combine-rows #(apply * (concat %1 %2))) (visible-ranges-for-rows rows) (visible-ranges-for-cols rows)))

(defn best-spot
  [rows]
  (->> (visible-ranges-combined rows)
       (flatten)
       (apply max)))
