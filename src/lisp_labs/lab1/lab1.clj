(ns lisp_labs.lab1.lab1)

(defn to-chars [alphabet]
  (if (string? alphabet)
    (seq alphabet)
    alphabet))

(defn last-ch [s]
  (if (pos? (.length s))
    (.charAt s (dec (.length s)))
    nil))

(defn chars-to-strings [chars]
  (if (empty? chars)
    '()
    (cons (str (first chars))
          (chars-to-strings (rest chars)))))

(defn generate-strings [alphabet n]
  (let [chars (to-chars alphabet)]
    (if (= n 0)
      '("")

      (if (= n 1)
        (chars-to-strings chars)

        (let [prefixes (generate-strings alphabet (dec n))]
          (for [p prefixes
                ch chars
                :when (let [last-symbol (last-ch p)]
                        (or (nil? last-symbol)
                            (not= last-symbol ch)))]
            (str p ch)))))))

(defn -main []
  (println (generate-strings "abc" 2)))