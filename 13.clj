(ns aoc2022.19
  (:refer-clojure :exclude [compare]))

(defn read-input
  [s]
  (let [spair->vectors
        (partial map read-string)]
    (->> (clojure.string/split s #"\n\n")
         (map clojure.string/split-lines)
         (map spair->vectors))))

(defn identify
  [x]
  (cond
    (sequential? x)
    :list

    (int? x)
    :int))

(def wrong-order 1)
(def right-order -1)
(def continue 0)

(defmulti compare
  (fn [x y]
    (mapv identify [x y])))

(defmethod compare [:int :int]
  [a b]
  (clojure.core/compare a b))

(defmethod compare [:list :int]
  [a b]
  (compare a [b]))

(defmethod compare [:int :list]
  [a b]
  (compare [a] b))

(defmethod compare [:list :list]
  [a b]
  (cond
    (and (empty? a) (empty? b))
    continue

    (empty? a)
    right-order

    (empty? b)
    wrong-order

    :else
    (let [c (compare (first a) (first b))]
      (if (= c continue)
        (compare (rest a) (rest b))
        c))))


(assert (= right-order
           (compare [1 1 3 1 1] [1 1 5 1 1])))

(assert (= right-order
           (compare [[1] [2 3 4]] [[1] 4])))

(assert (= wrong-order
           (compare [9] [[8 7 6]])))

(assert (= right-order
           (compare [[4 4] 4 4] [[4 4] 4 4 4])))

(assert (= wrong-order
           (compare [7 7 7 7] [7 7 7])))

(assert (= right-order
           (compare [] [3])))

(assert (= wrong-order
           (compare [[[]]] [[]])))

(assert (= wrong-order
           (compare [1,[2,[3,[4,[5,6,7]]]],8,9] [1,[2,[3,[4,[5,6,0]]]],8,9])))

(defn solve-puzzle
  [pairs]
  (->> (map (partial apply compare) pairs)
       (map vector (range))
       (filter (comp (partial = right-order) second))
       (map first)
       (map inc)
       (apply +)))

(defn solve-puzzle-again
  [pairs]
  (->>
       (-> (apply concat pairs)
           (conj [[2]])
           (conj [[6]]))
       (sort #(compare %1 %2))
       (map vector (range))
       (filter (comp #{[[2]] [[6]]} second))
       (map first)
       (map inc)
       (apply *)))


