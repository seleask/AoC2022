(ns aoc2022.3)

(defn common-item
  [c1 c2]
  (some (set c1) c2))

(defn item->int
  [item]
  (- (int item)
     (if (Character/isUpperCase item) 38 96)))

(defn split-bag
  [bag]
  (let [mid (quot (count bag) 2)]
    (split-at mid bag)))

(defn sum-priorities
  [bags]
  (let [bag-priority #(->> %
                           (split-bag)
                           (apply common-item)
                           (item->int))]
    (->> bags
         (map bag-priority)
         (apply +))))

(defn solve-puzzle
  [s]
  (-> s
      (clojure.string/split-lines)
      (sum-priorities)))

(defn group-common-item
  [b1 b2 b3]
  (common-item
   (clojure.set/intersection (set b1) (set b2))
   b3))

(defn sum-group-priorities
  [bags]
  (let [group-priority #(->> %
                           (apply group-common-item)
                           (item->int))]
    (->> bags
         (partition 3)
         (map group-priority)
         (apply +))))

(defn solve-puzzle-2
  [s]
  (-> s
      (clojure.string/split-lines)
      (sum-group-priorities)))
