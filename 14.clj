(ns aoc2022.14)

(defn pair->coords
  [[a-x a-y :as a] [b-x b-y :as b]]
  (let [x-diff (- b-x a-x)
        y-diff (- b-y a-y)
        x-line? (not (zero? x-diff))
        varying-diff (if x-line? x-diff y-diff)
        varying-pair (if x-line? [a-x b-x] [a-y b-y])
        range-increment (if (pos? varying-diff) 1 -1)
        values (range (first varying-pair) (+ range-increment (second varying-pair)) range-increment)]
    (if x-line?
      (map vector values (repeat a-y))
      (map vector (repeat a-x) values))))

(defn line->coords
  [pairs]
  (mapcat (partial apply pair->coords) (partition 2 1 pairs)))

(defn read-input
  [s]
  (let [parse-int #(Integer/parseInt %)
        parse-pair #(map parse-int (clojure.string/split % #","))
        parse-line (comp line->coords (partial map parse-pair) #(clojure.string/split % #"\ \->\ "))]
    (->> (clojure.string/split-lines s)
         (map parse-line))))

(defn into-sand-set
  [coords]
  (into #{} (map vec coords)))

(defn empty-translated-space?
  [translation sand-set coord]
   (let [coord (if (seq translation) (mapv #(%1 %2) translation coord) coord)]
     (when-not (contains? sand-set coord)
       coord)))

(def empty-space? (partial empty-translated-space? []))
(def empty-left-down? (partial empty-translated-space? [dec inc]))
(def empty-right-down? (partial empty-translated-space? [inc inc]))

(defn translate
  [translation coord]
  (mapv #(%1 %2) translation coord))

(def above (partial translate [identity dec]))
(def below (partial translate [identity inc]))
(def left-down (partial translate [dec inc]))
(def right-down (partial translate [inc inc]))

(defn try-and-place
  [sand-set coord]
  ;; obstacle here? go up
  (let [continue
        (fn [next-coord]
          (lazy-seq
           (cons coord
                 (try-and-place sand-set next-coord))))]
    (if (not (empty-space? sand-set coord))
      (continue (above coord))
      (cond
       ;; nothing beneath, so descend
        (empty-space? sand-set (below coord))
        (continue (below coord))

       ;; obstacle beneath? try downleft
        (empty-space? sand-set (left-down coord))
        (continue (left-down coord))

       ;; obstacle downleft? try downright
        (empty-space? sand-set (right-down coord))
        (continue (right-down coord))

       ;; place it here if empty and no space below
        :else
        (list coord)))))

(def test-data "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9")
(def test-sand-set (->> (read-input test-data) (apply concat) (into-sand-set)))

(assert (= [500 8]
           (last (try-and-place test-sand-set [500 9]))))
(assert (= [501 8]
           (last (try-and-place test-sand-set [502 3]))))
(assert (= [501 8]
           (last (try-and-place test-sand-set [502 3]))))

(defn out-of-bounds?
  [min-depth-bound max-depth-bound [_ depth]]
  (or
   (>= depth max-depth-bound)
   (< depth min-depth-bound)))

(defn place-until-out-of-bounds
  ([sand-set [_ source-depth :as source-pos] lowest-rocks settlement-counter]
   (let [fall-path (try-and-place sand-set source-pos)]
     (if (some (partial out-of-bounds? source-depth lowest-rocks) fall-path)
       settlement-counter
       (recur (conj sand-set (last fall-path)) source-pos lowest-rocks (inc settlement-counter)))))

  ([sand-set]
   (place-until-out-of-bounds
    sand-set
    [500 0]
    (apply max (map second sand-set))
    0)))

(defn solve-puzzle
  [s]
  (->> (read-input s)
       (apply concat)
       (into-sand-set)
       (place-until-out-of-bounds)))

(defn generate-floor-coords
  [sand-set]
  (let [max-depth (+ 2 (apply max (map second sand-set)))
        min-col 0
        max-col (+ (apply max (map first sand-set)) 1000)]
    (line->coords [[min-col max-depth] [max-col max-depth]])))

(defn solve-puzzle-again
  [s]
  (let [sand-set
        (->> (read-input s)
             (apply concat)
             (into-sand-set))]
    (-> (clojure.set/union sand-set (set (generate-floor-coords sand-set)))
        (place-until-out-of-bounds))))
