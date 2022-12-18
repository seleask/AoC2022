(ns aoc2022.15)

(defn read-input
  [s]
  (let [parse-int #(Integer/parseInt %)
        parse-line #(->> (re-seq #"(?:x|y)=([\d\-]+)" %)
                         (map last)
                         (map parse-int)
                         (partition 2)
                         (map vec))]
    (->> (clojure.string/split-lines s)
         (map parse-line))))

(defn sensor-intersection-depth
  [[x y] distance row-y]
  (let [[y-min y-max] [(- y distance) (+ y distance)]]
    (when (and (>= row-y y-min) (<= row-y y-max))
      (if (>= y row-y)
        (- (- y distance) row-y)
        (- (+ y distance) row-y)))))

(defn intersections-at-depth
  [sensor-x intersection-depth row-y]
  (let [depth (Math/abs intersection-depth)]
    (for [x (range (- sensor-x depth) (inc (+ sensor-x depth)))]
      [x row-y])))

(defn intersects-y?
  [row-y [[x y :as sensor] distance]]
  (when-let [depth (sensor-intersection-depth sensor distance row-y)]
    (intersections-at-depth x depth row-y)))

(defn manhattan-distance
  [a b]
  (->> (map (comp #(Math/abs %) -) a b)
       (apply +)))

(defn to-sensor-distance-pair
  [sensor-beacon-pair]
  (-> sensor-beacon-pair
      (vec)
      (assoc 1 (apply manhattan-distance sensor-beacon-pair))))

(defn solve-puzzle
  [sensor-beacon-pairs]
  (let [beacons
        (map second sensor-beacon-pairs)]
    (->> sensor-beacon-pairs
         (map to-sensor-distance-pair)
         (keep (partial intersects-y? 2000000))
         (apply concat)
         (remove (set beacons))
         (distinct)
         (count))))

(defn opposing-with-gap?
  [sd-pair-a sd-pair-b]
  (let [[a-pos a-range] sd-pair-a
        [b-pos b-range] sd-pair-b
        dist (manhattan-distance a-pos b-pos)]
    (= (- dist 2) (+ a-range b-range))))

(defn intersects?
  [sd-pair-a sd-pair-b]
  (let [[a-pos a-range] sd-pair-a
        [b-pos b-range] sd-pair-b
        dist (manhattan-distance a-pos b-pos)]
    (< dist (+ a-range b-range))))

(defn intersecting-pairs?
  [opposing-pair-a opposing-pair-b]
  (every? (partial apply intersects?) (map vector opposing-pair-a opposing-pair-b)))

;; does this general pattern describe the solution?

;; from the example
;; two pairs of opposing sensors with a 1 cell gap between
(assert (opposing-with-gap? [[12 14] 4] [[16 7] 5]))
(assert (opposing-with-gap? [[8 7] 9] [[20 14] 8]))
;; each pair intersects on the non-opposing orientations
(assert (intersects? [[12 14] 4] [[8 7] 9]))
(assert (intersects? [[16 7] 5] [[20 14] 8]))
;; tautological but whatever
(assert (not (intersects? [[12 14] 4] [[16 7] 5])))
(assert (not (intersects? [[8 7] 9] [[20 14] 8])))

(defn pairwise-product
  [xs]
  (let [splits (map #(split-at % xs) (range (count xs)))
        split->pairs (fn [[_ tail]] (map (partial vector (first tail)) (rest tail)))]
    (mapcat split->pairs splits)))

(defn find-opposing-sensors
  [sd-pairs]
  (->> sd-pairs
       (pairwise-product)
       (filter (partial apply opposing-with-gap?))))

(defn find-pairs-of-opposing-pairs
  [opposing-pairs]
  (->> opposing-pairs
       (pairwise-product)
       (filter (partial apply intersecting-pairs?))))

(defn perimeter-coords
  [[x y] distance]
  (let [apply-gradient
        (fn [i j]
          (map
           (fn [a b] [(i x a) (j y b)])
           (range (inc distance))
           (reverse (range (inc distance)))))]
    (set
     (mapcat
      (partial apply apply-gradient)
      [[+ +] [+ -] [- +] [- -]]))))

(defn gap-points
  [sd-pair-a sd-pair-b]
  (let [perimeter-a (apply perimeter-coords (update sd-pair-a 1 inc))
        perimeter-b (apply perimeter-coords (update sd-pair-b 1 inc))]
    (clojure.set/intersection perimeter-a perimeter-b)))

(defn find-candidate-points
  [opposing-pair-a opposing-pair-b]
  (apply clojure.set/intersection
         (map (partial apply gap-points) [opposing-pair-a opposing-pair-b])))

;; valid solution if not contained by any sensor-beacon pair
(defn valid-solution?
  [sensor-distance-pairs pos]
  (let [contains-pos?
        (fn [[sensor distance]]
          (<= (manhattan-distance sensor pos) distance))]
  (every?
   #(not (contains-pos? %))
   sensor-distance-pairs)))

(defn solve-puzzle-again
  [sensor-beacon-pairs]
  (let [sensor-distance-pairs
       (map to-sensor-distance-pair sensor-beacon-pairs)

        tuning-freq
        (fn [[x y]]
          (+ (* 4000000 x) y))]
  (->> sensor-distance-pairs
       (find-opposing-sensors)
       (find-pairs-of-opposing-pairs)
       (mapcat (partial apply find-candidate-points))
       (filter (partial valid-solution? sensor-distance-pairs))
       (first)
       (tuning-freq))))
