(ns aoc2022.9)

(defn diff
  [pos-a pos-b]
  (->> (map vector pos-a pos-b)
       (map #(- (first %) (second %)))))

(defn not-adjacent-to?
  [pos-a pos-b]
  (let [d (diff pos-a pos-b)]
    (when (some (partial < 1)
                (map #(Math/abs %) d))
      d)))

;; nightmare cond
(defn diff->tail-move
  [[x-diff y-diff]]
  (let [x-diff? (> (Math/abs x-diff) 1)
        y-diff? (> (Math/abs y-diff) 1)]
    (cond
      (and x-diff? y-diff?) ; diagonal movement, only when tail follows tail
      [(quot x-diff 2) (quot y-diff 2)]

      (and x-diff? (= (Math/abs y-diff) 1))
      [(quot x-diff 2) y-diff]

      (and y-diff? (= (Math/abs x-diff) 1))
      [x-diff (quot y-diff 2)]

      x-diff?
      [(quot x-diff 2) 0]

      :else
      [0 (quot y-diff 2)])))

(defn next-tail-pos
  [head-pos tail-pos]
  (or
   (some->> (not-adjacent-to? head-pos tail-pos)
            (diff->tail-move)
            (mapv + tail-pos))
   tail-pos))

(defn apply-move
  [pos move]
  (->>
   (case move
     "L" [0 -1]
     "R" [0 1]
     "U" [-1 0]
     "D" [1 0])
   (mapv + pos)))

(defn execute-head-move
  [[head-pos tail-pos] move]
  (let [new-head-pos (apply-move head-pos move)
        new-tail-pos (next-tail-pos new-head-pos tail-pos)]
    [new-head-pos new-tail-pos]))

(defn adjust-segments
  [head segments]
  (reduce
   (fn [updated-segments segment]
     (let [previous-segment (last updated-segments)
           new-segment-pos (next-tail-pos previous-segment segment)]
       (conj updated-segments new-segment-pos)))
   [head]
   segments))

(defn execute-move
  [segments move]
  (let [[head & tail] segments
        new-head-pos (apply-move head move)]
    (adjust-segments new-head-pos tail)))

(defn repeated-move
  [move n]
  (repeat n move))

(defn execute-moves
  [initial-state moves]
  (reductions
   (fn [segments move]
     (execute-move segments move))
   initial-state
   moves))

(defn lines->moves
  [lines]
  (mapcat (partial apply repeated-move) lines))

(defn visited-positions
  [states]
  (->> (map last states)
       (set)))

(def initial-segment-positions (repeat [0 0]))
(def nine-tail-snake (take 10 initial-segment-positions))

(defn solve-puzzle
  [lines]
  (->> (lines->moves lines)
       (execute-moves nine-tail-snake)
       (visited-positions)
       (count)))

(defn read-input
  [s]
  (let [parse-int #(Integer/parseInt %)
        parse-line #(-> (clojure.string/split % #"\ ") (vec) (update 1 parse-int))]
  (->> (clojure.string/split-lines s)
       (map parse-line))))

;; this was painful to debug for part 2, lesson learned to test the foundational logic properly.
;; resorted to printing

(defn blank-grid
  [segments]
  (let [top-i (apply max (map first segments))
        bottom-i (apply min (map first segments))
        top-j (apply max (map second segments))
        bottom-j (apply min (map second segments))
        rows (inc (- top-i bottom-i))
        cols (inc (- top-j bottom-j))]
    (->> (repeat ".")
         (take cols)
         (vec)
         (repeat)
         (take rows)
         (vec))))

(defn fill-grid
  [segments]
  (let [min-i (apply min (map first segments))
        min-j (apply min (map second segments))]
  (reduce
   (fn [grid [label [i j]]]
     (assoc-in grid [(- i min-i) (- j min-j)] label))
   (blank-grid segments)
   (reverse
    (map vector (cons "H" (map str (map inc (range))))
        segments)))))

(defn print-grid
  [rows]
  (doseq [row rows]
    (println (apply str row))))

(defn print-state
  [state]
  (->> (fill-grid state)
       (print-grid)))
