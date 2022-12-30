(ns aoc2022.16)

(defn read-input
  [s]
  (let [parse-int #(Integer/parseInt %)
        parse-valves #(re-seq #"[A-Z]+" %)
        parse-valve #(rest (re-matches #"Valve\ ([A-Z]+).*rate=(\d+)" %))
        parse-line
        (fn [l]
          (let [[valve-string valves-string]
                (clojure.string/split l #";")

                [valve pressure] (parse-valve valve-string)
                valves (parse-valves valves-string)]
            [valve
             {:pressure (parse-int pressure)
              :valves valves}]))]
    (->> (clojure.string/split-lines s)
         (map parse-line)
         (into {}))))

(defn fw-base-case
  [input-graph]
  (let [nodes (sort (set (keys input-graph)))]
    (reduce
     (fn [g [i j]]
       (cond
         (= i j)
         (assoc-in g [i j] 0)

         (contains? (set (:valves (input-graph i))) j)
         (assoc-in g [i j] 1)

         :else
         (assoc-in g [i j] Integer/MAX_VALUE)))

     (into (sorted-map) (map vector nodes (repeat (sorted-map))))
     (for [i nodes
           j nodes]
       [i j]))))

(defn fw-recursive-case
  [fw-graph k]
  (reduce
   (fn [g [i j]]
     (let [ij (get-in g [i j])
           ik (get-in g [i k])
           kj (get-in g [k j])]
       (if (> ij (+ ik kj))
         (assoc-in g [i j] (+ ik kj))
         g)))

   fw-graph
   (for [i (keys fw-graph)
         j (keys fw-graph)]
     [i j])))

(defn floyd-warshall
  [input-graph]
  (reduce
   fw-recursive-case
   (fw-base-case input-graph)
   (sort (keys input-graph))))

(defn join-graphs
  [fw-graph input-graph]
  (reduce
   (fn [g [from to->dist]]
     (assoc-in g [from :distances] to->dist))
   input-graph
   fw-graph))

;; empty if can't be extended within time limit
(defn path-extensions
  [graph {:keys [open-valves closed-valves time-left location score]}]
    (for [next-valve closed-valves
          :let [distance (get-in graph [location :distances next-valve])]
          :when (or
                 (>= (- time-left distance 1) 0)
                 (empty? closed-valves))]
      {:location next-valve
       :score (+ score
                 (* (- time-left distance 1) (get-in graph [next-valve :pressure])))
       :time-left (- time-left distance 1)
       :open-valves (conj open-valves next-valve)
       :closed-valves (disj closed-valves next-valve)}))

(defn terminal-path?
  [{:keys [time-left closed-valves]}]
  (or
   (<= time-left 2)
   (empty? closed-valves)))

(defn find-best-terminal-path-for-closed-valves-with-score-cache
  [graph time-limit closed-valves]
  (let [open-valves
        (clojure.set/difference (set (keys graph)) (set closed-valves))]

  (loop [stack [{:time-left time-limit
                 :open-valves open-valves
                 :closed-valves (clojure.set/difference (set closed-valves) open-valves)
                 :location "AA"
                 :score 0}]
         closed-valves->best-score {}]

    (let [path-data (peek stack)]
      (cond
        (nil? path-data)
        closed-valves->best-score

        (terminal-path? path-data)
        (recur (pop stack)
               (update-in closed-valves->best-score
                          [(set (:closed-valves path-data))]
                          #(if (nil? %) (:score path-data) (max % (:score path-data)))))

        :else
        (recur
         (into (pop stack) (path-extensions graph path-data))
         (update-in closed-valves->best-score
                    [(set (:closed-valves path-data))]
                    #(if (nil? %) (:score path-data) (max % (:score path-data))))))))))

(def find-best-terminal-path-for-closed-valves (comp (partial apply max) vals find-best-terminal-path-for-closed-valves-with-score-cache))

(defn find-best-terminal-path
  [graph]
  (let [zero-valves
        (->> (filter (comp zero? :pressure second) graph)
             (map first))]

  (find-best-terminal-path-for-closed-valves graph 30 (clojure.set/difference (set (keys graph)) (set zero-valves)))))

(defn solve-puzzle
  [s]
  (let [input-graph (read-input s)
        fw-graph (floyd-warshall input-graph)
        best-score (find-best-terminal-path (join-graphs fw-graph input-graph))]
    best-score))

(defn find-best-parallel-terminal-paths
  [graph]
  (let [zero-valves
        (->> (filter (comp zero? :pressure second) graph)
             (map first))

        closed-valves
        (clojure.set/difference (set (keys graph)) (set zero-valves))

        remaining-closed-valves->score
        (find-best-terminal-path-for-closed-valves-with-score-cache graph 26 closed-valves)]

     (for [[elephant-valves person-score] remaining-closed-valves->score]
       (+
        person-score
        (find-best-terminal-path-for-closed-valves graph 26 elephant-valves)))))

(defn solve-puzzle-again
  [s]
  (let [input-graph (read-input s)
        fw-graph (floyd-warshall input-graph)
        graph (join-graphs fw-graph input-graph)]
    (reduce max 0 (find-best-parallel-terminal-paths graph))))
