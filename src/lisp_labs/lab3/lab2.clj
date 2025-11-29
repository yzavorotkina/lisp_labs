(ns lisp-labs.lab3.lab2)

(defn normal-filter
  [pred coll]
  (filter pred coll))

(defn heavy-pred?
  [x]
  (let [x (double x)]
    (letfn [(step [i acc]
              (if (< i 5000)
                (recur (inc i)
                       (+ acc
                          (Math/sin (+ x i))
                          (Math/cos (- x i))))
                acc))]
      (> (step 0 0.0) 0.0))))

(defn lazy-par-filter
  "Ленивый параллельный filter.
   Для каждого элемента coll создаётся future, который считает pred
   и возвращает либо сам элемент, либо nil. Затем keep + deref
   лениво вытаскивают результаты и отбрасывают nil."
  [pred coll]
  (let [futures (map (fn [x]
                       (future
                         (when (pred x)
                           x)))
                     coll)]
    (keep deref futures)))

(defn demo-lazy!
  ([] (demo-lazy! 20000))
  ([N]
   (println "N =" N)

   (let [data (range 2 (inc N))]
     (println "\nОбычный ленивый filter (конечный диапазон):")
     (let [seq-result
           (time
             (doall (normal-filter heavy-pred? data)))]

       (println "Найдено (seq):" (count seq-result)))

     (println "\nЛенивый параллельный filter (конечный диапазон):")
     (let [par-result
           (time
             (doall (lazy-par-filter heavy-pred? data)))]

       (println "Найдено (par):" (count par-result))
       (println "Результаты совпадают?" (= (set par-result)
                                           (set (normal-filter heavy-pred? data))))))

   (println "\nБесконечная последовательность:")
   (let [k 200
         inf-result
         (time
           (doall
             (take k
                   (lazy-par-filter heavy-pred? (range)))))]

     (println "Результат:" inf-result))

   :ok))

(defn -main
  [& args]
  (let [[n-str] args
        N (if n-str (Integer/parseInt n-str) 20000)]
    (demo-lazy! N)
    (shutdown-agents)))
