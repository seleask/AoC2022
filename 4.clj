(ns aoc2022.4)

(defn range-contains-completely?
  [[bot1 top1] [bot2 top2]]
  (and
   (>= bot2 bot1)
   (<= top2 top1)))

(defn range-length
  [[bot top]]
  (- (inc top) bot))

(defn pair-has-complete-containment?
  [r1 r2]
  (let [[larger-range smaller-range]
        (if (> (range-length r1) (range-length r2))
          [r1 r2]
          [r2 r1])]
    (range-contains-completely? larger-range smaller-range)))

(defn count-complete-containments
  [ranges]
  (->> ranges
       (filter (partial apply pair-has-complete-containment?))
       (count)))

(defn pair-overlaps?
  [[bot1 top1] [bot2 top2]]
  (and
   (<= bot2 top1)
   (>= top2 bot1)))

(defn counted-overlaps
  [ranges]
  (->> ranges
       (filter (partial apply pair-overlaps?))
       (count)))

(defn read-range-pairs
  [s]
  (let [read-range-pair #(->> %
                              (re-matches #"(\d+)\-(\d+),(\d+)\-(\d+)")
                              (rest)
                              (map (fn [i] (Integer/parseInt i)))
                              (partition 2))]

    (->> s
         (clojure.string/split-lines)
         (map read-range-pair))))
