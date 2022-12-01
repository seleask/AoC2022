(ns aoc2022.1
  (:require [clojure.string :refer [split-lines split]]))

(defn parse-food-items
  [s]
  (->> s
       (split-lines)
       (map #(Integer/parseInt %))))

(defn read-problem
  [s]
  (->> (split s #"\n\n")
       (map parse-food-items)))

(def elf-calories (partial apply +))

(defn highest-calorie-elf
  [elf-calorie-sets]
  (->> elf-calorie-sets
       (map elf-calories)
       (apply max)))

(defn add-to-ordered-seq
  [cmp limit xs item]
  (if (and (empty? xs) (> limit 0))
    (conj xs item)
    (let [result (if-not (cmp item (first xs))
                   (conj xs item)
                   (conj (add-to-ordered-seq cmp limit (rest xs) item) (first xs)))]
      (if (> (count result) limit)
        (rest result)
        result))))

(def add-to-top-three (partial add-to-ordered-seq >= 3))

(defn three-highest-calorie-elves
  [elf-calorie-sets]
  (reduce
   (fn [top-three calorie-set]
     (let [calories (elf-calories calorie-set)]
       (add-to-top-three top-three calories)))
   '()
   elf-calorie-sets))

(defn find-top-three
  [puzzle-input]
  (->> (read-problem puzzle-input)
       (three-highest-calorie-elves)
       (apply +)))
