(ns lisp-labs.lab1.lab4)

(defn to-chars [alphabet]
  (if (string? alphabet)
    (seq alphabet)
    alphabet))

(defn last-ch [s]
  (if (pos? (.length s))
    (.charAt s (dec (.length s)))
    nil))

(defn allowed? [prefix ch]
  (let [last-symbol (last-ch prefix)]
    (or (nil? last-symbol)
        (not= last-symbol ch))))

(defn extend-one [prefix chars]
  (->> chars
       (filter (fn [ch] (allowed? prefix ch)))
       (map    (fn [ch] (str prefix ch)))))


(defn extend-all [prefixes chars]
  (reduce
    (fn [acc prefix]
      (concat acc (extend-one prefix chars)))
    '()
    prefixes))

(defn generate-strings-map [alphabet n]
  (let [chars (to-chars alphabet)]
    (cond
      (= n 0)
      '("")

      (= n 1)
      (map (fn [ch] (str ch)) chars)

      :else
      (let [prefixes (generate-strings-map alphabet (dec n))]
        (extend-all prefixes chars)))))

(defn -main []
  (println (generate-strings-map "abc" 3)))