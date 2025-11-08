(ns lisp-labs.lab1.lab3)

(defn my-map [f xs]
  (if (empty? xs)
    '()
    (cons (f (first xs))
          (my-map f (rest xs)))))

(defn my-filter [pred xs]
  (if (empty? xs)
    '()
    (let [head (first xs)
          tail (rest xs)]
      (if (pred head)
        (cons head (my-filter pred tail))
        (my-filter pred tail)))))

(defn -main [& args]
  (println "my-map:" (my-map inc '(1 2 3)))
  (println "my-filter:" (my-filter odd? '(1 2 3 4))))