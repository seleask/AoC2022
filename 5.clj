(ns aoc2022.5)

(defn execute-move
  [stacks part-two? amount from to]
  (let [f
        (if part-two? identity reverse)

        [moving-blocks staying-blocks]
        (split-at amount (get stacks (dec from)))]

    (-> stacks
        (assoc (dec from) staying-blocks)
        (update (dec to) #(concat (f moving-blocks) %)))))

(defn read-stacks
  [s]
  (let [lines (clojure.string/split-lines s)
        layers (butlast lines)
        transpose #(apply mapv vector %)
        stack? #(second (re-matches #"\s*(\w+)" %))]
    (->> (transpose layers)
         (map (partial apply str))
         (map stack?)
         (remove nil?)
         (vec))))

(defn read-moves
  [s]
  (let [lines (clojure.string/split-lines s)
        move-matcher #(re-seq #"\d+" %)
        parse-int #(Integer/parseInt %)]
    (->> lines
         (map move-matcher)
         (map #(map parse-int %)))))

(defn read-puzzle
  [s]
  (let [[stacks-string moves-string]
        (clojure.string/split s #"\n\n")
        stacks (read-stacks stacks-string)
        moves (read-moves moves-string)]
    [stacks moves]))

(defn solve-puzzle
  [part-two? s]
  (let [[stacks moves] (read-puzzle s)]
    (->>
     (reduce
      (fn [stacks move]
        (apply (partial execute-move stacks part-two?) move))
      stacks
      moves)
     (map first)
     (apply str))))
