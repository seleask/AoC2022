(ns aoc2022.11)

(def parse-int #(bigint %))

(defn trim-var-decl
  [line]
  (-> line
      (clojure.string/split #":")
      (second)
      (clojure.string/trim)))

(defn split-spaces
  [s]
  (clojure.string/split s #"\ "))

(defn parse-items
  [s]
  (->> (clojure.string/split s #",\ ")
       (map parse-int)))

(defn parse-operation
  [s]
  (let [[_ _ _ op operand] (split-spaces s)]
    [op "old" operand]))

(defn parse-divisible
  [s]
  (->> (split-spaces s) (last) (parse-int)))

(defn parse-throw
  [s]
  (->> (split-spaces s) (last) (parse-int)))

(defn op-vec->fn
  [[op _ x]]
  #((eval (symbol op)) % (if (= x "old") % (parse-int x))))

(defn parse-monkey
  [s]
  (let [[items-line
         operation-line
         divisible-line
         true-line
         false-line] (map trim-var-decl (rest (clojure.string/split-lines s)))]
    {:items (parse-items items-line)
     :operation (parse-operation operation-line)
     :op-fn (op-vec->fn (parse-operation operation-line))
     :test
     {:divisor (parse-divisible divisible-line)
      :true-outcome (parse-throw true-line)
      :false-outcome (parse-throw false-line)}}))

(defn item->remainders
  [divisors item]
  ;(mapv (partial mod item) divisors))
  (into {} (map #(vector % (mod item %)) divisors)))

(defn replace-items-with-remainder-vecs
  [monkeys]
  (let [divisors (map (comp :divisor :test) monkeys)]
    (mapv
     (fn [monkey]
       (update monkey :items #(map (partial item->remainders divisors) %)))
     monkeys)))

(defn setup-monkeys
  [s]
  (->> (clojure.string/split s #"\n\n")
       (map parse-monkey)
       (map #(assoc % :inspection-count 0))
       (replace-items-with-remainder-vecs)
       (vec)))

(defn map-m
  [m f]
  (into {} (map (fn [[k v]]
                  (f k v))
                (seq m))))

;; item is now remainders for part 2, no longer dividing by 3
;; couldn't quite figure out how to get part 1 to work with the remainders representation
(defn process-item
  [item op-fn divisor true-out false-out]
  (let [updated-remainders (-> (map-m item (fn [k v]
                                             [k (mod (op-fn v) k)]))
                               (update divisor #(mod % divisor)))]
    (if (= (get updated-remainders divisor) 0)
      [updated-remainders true-out]
      [updated-remainders false-out])))

(defn process-turn
  [monkey]
  (let [{:keys [items op-fn test]} monkey
        {:keys [divisor true-outcome false-outcome]} test]
    (map #(process-item % op-fn divisor true-outcome false-outcome) items)))

(defn dispatch-item-pairs
  [monkeys item-monkey-pairs]
  (reduce
   (fn [monkeys [item monkey-id]]
     (update-in monkeys [monkey-id :items] conj item))
   monkeys
   item-monkey-pairs))

(defn process-round
  [monkeys]
  (reduce
   (fn [monkeys monkey-id]
     (let [monkey (get monkeys monkey-id)
           item-monkey-id-pairs (process-turn monkey)]
       (-> monkeys
           (dispatch-item-pairs item-monkey-id-pairs)
           (update-in [monkey-id :inspection-count] + (count item-monkey-id-pairs))
           (assoc-in [monkey-id :items] []))))
   monkeys
   (range (count monkeys))))

(defn solve-puzzle
  [s]
  (->>
   (nth
    (->> (setup-monkeys s)
         (iterate process-round))
      ;  20)
    10000)
   (map :inspection-count)
   (sort)
   (reverse)
   (take 2)
   (apply *)))
