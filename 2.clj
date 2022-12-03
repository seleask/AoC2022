(ns aoc2022.2
  (:require [clojure.string :refer [split-lines split]]))

(def n-possible-moves 3)

(def lh-moves "ABC")
(def rh-moves "XYZ")
(def lh-move->score (zipmap (map str lh-moves) (range 1 (inc n-possible-moves))))
(def rh-move->score (zipmap (map str rh-moves) (range 1 (inc n-possible-moves))))
(def move->score (merge lh-move->score rh-move->score))

(defn beats?
  [x y n]
  (if (= x 1)
    (= y n)
    (= x (inc y))))

(defn draws?
  [x y]
  (= x y))

(defn score-round
  [[l r]]
  (let [[x y] (map move->score [l r])]
    (cond
      (draws? y x) (+ 3 y)
      (beats? y x n-possible-moves) (+ 6 y)
      :else y)))

(defn read-puzzle
  [s]
  (let [read-round #(split % #" ")]
    (->> s
         (split-lines)
         (map read-round))))

(defn solve-puzzle
  [p]
  (->> p
       (map score-round)
       (apply +)))

(defn determine-move
  [[l r]]
  (let [y (- (move->score r) 2) ;; convert the outcome 1 to 3 range to -1 to 1
        x (-> l
              (move->score)
              (+ y) ;; apply outcome to index: right of always wins, left of always loses,
                    ;; draw is same index
              (dec) ;; convert to zero index
              (mod n-possible-moves)
              (inc)) ;; back to one index
        score->move (clojure.set/map-invert rh-move->score)]
    (score->move x)))

(defn replace-right-with-correct-move
  [[l r]]
  [l (determine-move [l r])])

(defn solve-puzzle-2
  [p]
  (->> p
       (map replace-right-with-correct-move)
       (solve-puzzle)))
