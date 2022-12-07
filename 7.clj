(ns aoc2022.7)

(defn command-line?
  [[token & _]]
  (= token "$"))

(defn cd?
  [[_ cmd & _dir :as line]]
  (and (command-line? line)
       (= cmd "cd")
       (seq line)))

(defn numeric?
  [s]
  (re-matches #"[\d]+" s))

(defn file-listing->metadata
  [line]
  (let [[size-or-dir filename]
        line

        type
        (if (numeric? size-or-dir) :file :dir)]

    (cond-> {:type type
             :name filename}
      (= type :file) (assoc :size (Integer/parseInt size-or-dir)))))

(defn extract-pwd-per-line
  [log-lines]
  (reductions
   (fn [path log-line]
     (if (cd? log-line)
       (let [dir (last log-line)]
         (if (= ".." dir)
           (pop path)
           (conj path dir)))
       path))
   []
   log-lines))

(defn extract-file-metadata-per-line
  [log-lines]
  (map
   #(when (not (command-line? %))
      (file-listing->metadata %))
   log-lines))

(defn extract-metadata
  [log-lines]
  (->> log-lines
       ((juxt extract-file-metadata-per-line extract-pwd-per-line))
       (apply (partial map vector))
       (filter (comp some? first))
       (map (partial apply #(assoc %1 :path %2)))))

(defn build-tree
  [file-metadata-list]
  (let [children->attributes (fn [children] {:children children})]
    (->> file-metadata-list
         (group-by :path)
         (map #(update-in % [1] children->attributes))
         (into {}))))

(defn lookup-dir-size
  [tree dir]
  (let [dir-path (conj (:path dir) (:name dir))
        result (-> (get tree dir-path) (get :size 0))]
    result))

(defn annotate-with-dir-sizes
  [tree]
  (let [dir-paths (->> tree (keys) (sort-by count) (reverse))]
    (reduce
     (fn [tree dir-path]
       (let [children (->> (get tree dir-path) :children (group-by :type) (into {}))
             files (get children :file)
             sub-dirs (get children :dir)
             files-size (apply + (map :size files))
             dirs-size (apply + (map (partial lookup-dir-size tree) sub-dirs))
             size (+ files-size dirs-size)]
         (assoc-in tree [dir-path :size] size)))
     tree
     dir-paths)))

(defn read-input
  [s]
  (->> (clojure.string/split-lines s)
       (map #(clojure.string/split % #"\ "))))

(defn puzzle-input->tree
  [s]
   (-> (read-input s)
       (extract-metadata)
       (build-tree)
       (annotate-with-dir-sizes)))

(defn solve-part-1
  [s]
  (->> (puzzle-input->tree s)
       (vals)
       (map :size)
       (filter #(<= % 100000))
       (apply +)))

(defn solve-part-2
  [s]
  (let [tree
        (puzzle-input->tree s)

        used-space
        (-> (get tree ["/"]) :size)

        necessary-unused-space
        30000000

        total-disk-space
        70000000

        free-disk-space
        (- total-disk-space used-space)

        size-to-remove
        (- necessary-unused-space free-disk-space)]

    (->> tree
         (vals)
         (map :size)
         (filter #(>= % size-to-remove))
         (sort)
         (first))))
