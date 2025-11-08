(ns lisp_labs.lab1.lab2)

(defn to-chars [alphabet]
  (if (string? alphabet)
    (seq alphabet)
    alphabet))

(defn last-ch [s]
  (if (pos? (.length s))
    (.charAt s (dec (.length s)))
    nil))

(defn chars->str1 [chs]
  (map (fn [c] (str c)) chs))

(defn allowed? [^String s c]
  (let [lc (last-ch s)]
    (or (nil? lc)
        (not= lc (first c)))))


(defn extend-string [chars acc ^String s]
  (into acc
        (for [c chars
              :when (allowed? s c)]
          (str s c))))

(defn build-next [frontier chars]
  (reduce
    (fn [acc s]
      (extend-string chars acc s))
    []
    frontier))

(defn all-strings-tail [alphabet n]
  (let [chars (-> alphabet to-chars chars->str1)
        seed  (vec chars)]
    (cond
      (= n 0)
      [""]

      (= n 1)
      seed
      :else
      (loop [frontier seed
             k        n]
        (if (<= k 1)
          frontier
          (recur (build-next frontier chars)
                 (dec k)))))))

(defn -main []
  (println (all-strings-tail "abc" 2)))
