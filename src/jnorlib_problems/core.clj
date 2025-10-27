(ns jnorlib-problems.core
  "Problems solved with code for fun.")

(defn- matrix?
  "Checks whether `x` is a rectangular matrix.
  In this context, a rectangular matrix is such if it is a vector of vectors
  where all the subvectors share the same length."
  [x]
  (cond
    (not (vector? x)) false
    (empty? x) true
    (some #(not (vector? %)) x) false
    :else (let [first-vec-count (count (first x))]
            (if (some #(not= (count %) first-vec-count) x)
              false
              true))))

(defn diagonal
  "Returns a sequence with the items of the main diagonal of `matrix`.
  See also [[matrix?]]."
  [matrix]
  (cond
    (not (matrix? matrix)) (throw (IllegalArgumentException. "matrix is not a vector of vectors with shared length."))
    (empty? matrix) (lazy-seq [])
    :else (->> (range 0 (min (count matrix) (count (first matrix))))
               (map #(-> matrix (nth %) (nth %))))))

(defn mode
  "Determines the most common items from the provided `coll`."
  [coll]
  (cond
    (not (coll? coll)) (throw (IllegalArgumentException. "coll must be a collection"))
    (empty? coll) (lazy-seq [])
    :else (let [entries (->> coll
                             (group-by identity)
                             (map (fn [[k vs]] [k (count vs)]))

                             ;; Coll of [k count]s.
                             (sort #(- (compare (last %1) (last %2)))))]
            (->> entries
                 (filter #(= (last %) (-> entries first last)))
                 (map first)))))
