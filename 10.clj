(ns aoc2022.10)

(defn eval-add
  [state x]
  (+ state x))

(defn eval
  [state [inst & args]]
  (case inst
    "add" (eval-add state (Integer/parseInt (first args)))
    "noop" state))

(defn expand-addx
  [x]
  [["noop"]
   ["add" x]])

(defn add-x?
  [[inst & _]]
  (= inst "addx"))

(defn expand-multi-cycles
  [instructions]
  (mapcat
   (fn [instruction]
     (if (add-x? instruction)
       (expand-addx (second instruction))
       [instruction]))
   instructions))

(defn eval-program
  [instructions]
  (->> (expand-multi-cycles instructions)
       (reductions eval 1)))

(defn read-input
  [s]
  (->> s
       (clojure.string/split-lines)
       (map #(clojure.string/split % #"\ "))))

(def signal-strength *)

(defn signal-strengths
  [cycles]
  (let [strengths (vec (map-indexed (fn [idx v] (signal-strength (inc idx) v)) cycles))]
    (map (partial get strengths)
         [19 59 99 139 179 219]))) ;; I hate mixing 1 and 0 indexing

(defn solve-puzzle
  [instructions]
  (->> (eval-program instructions)
       (signal-strengths)
       (apply +)))

(defn visible?
  [cycle sprite-pos]
  (contains? #{(dec sprite-pos)
               sprite-pos
               (inc sprite-pos)}
             (mod cycle 40)))

(defn draw-pixel
  [cycle sprite-pos]
  (if (visible? cycle sprite-pos)
    "#"
    "."))

(defn draw-all
  [cycle-sprite-pos-pairs]
  (map (partial apply draw-pixel) cycle-sprite-pos-pairs))

(defn print-screen
  [program-trace]
  (->> (map vector (range) program-trace)
       (draw-all)
       (partition 40)
       (map (partial apply str))
       (map println)
       (dorun)))
