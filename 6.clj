(ns aoc2022.6)

(def window-size 14) ;; 4 for part 1

(defn initial-state
  [s]
  (let [counts (frequencies (take window-size s))
        non-uniques (set
                     (map first
                          (filter (comp #(> % 1) second) counts)))]
    {:non-uniques non-uniques
     :counts counts}))

(defn del-char
  [state c]
  (cond-> state
    (= 2 (get-in state [:counts c]))
    (update :non-uniques disj c)

    true
    (update :counts
            #(if (= (get % c) 1)
               (dissoc % c)
               (update % c dec)))))

(defn add-char
  [state c]
  (cond-> state
    (= 1 (get-in state [:counts c]))
    (update :non-uniques conj c)

    true
    (update-in [:counts c] (fnil inc 0))))

(defn next-state
  [state going coming]
  (-> state
      (del-char going)
      (add-char coming)))

(defn extract-comings-and-goings
  [s]
  (->> (partition (inc window-size) 1 s)
       (map (juxt first last))))

(defn find-start-of-packet-index
  [s]
  (reduce
   (fn [state [i [going coming]]]
     (if (empty? (:non-uniques state))
       (reduced (+ i window-size))
       (next-state state going coming)))
   (initial-state s)
   (map-indexed (fn [idx pair] [idx pair])
                  (extract-comings-and-goings s))))
