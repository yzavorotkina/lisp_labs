(ns lisp-labs.lab3.lab1)

(defn chunked
  [coll block-size]
  (lazy-seq
    (when (seq coll)
      (let [chunk  (take block-size coll)
            rest-s (drop block-size coll)]
        (cons (vec chunk) (chunked rest-s block-size))))))

(defn par-filter-blocked
  [pred coll block-size]
  (when (pos? block-size)
    (let [blocks  (chunked coll block-size)
          futures (mapv
                    (fn [block]
                      (future (doall (filter pred block))))
                    blocks)]
      (apply concat (map deref futures)))))

(defn normal-filter
  [pred coll]
  (filter pred coll))

(defn heavy-pred?
  [x]
  (let [x (double x)
        acc (loop [i 0
                   acc 0.0]
              (if (< i 5000)
                (recur (inc i)
                       (+ acc
                          (Math/sin (+ x i))
                          (Math/cos (- x i))))
                acc))]
    (> acc 0.0)))

(defn demo!
  ([] (demo! 200000 4000))
  ([N block-size]
   (println "Число элементов:" N "  Размер блока:" block-size)
   (let [data (range 2 (inc N))]

     (println "\nПоследовательный filter:")
     (time (def seq-result (doall (normal-filter heavy-pred? data))))

     (println "\nПараллельный filter (future):")
     (time (def par-result (doall (par-filter-blocked heavy-pred? data block-size))))

     (println "\nРезультаты совпадают?" (= seq-result par-result))
     (println "Найдено элементов:" (count par-result))
     :ok)))

(defn -main
  [& args]
  (let [[n-str b-str] args
        N (if n-str (Integer/parseInt n-str) 200000)
        B (if b-str (Integer/parseInt b-str) 40000)]
    (demo! N B)
    (shutdown-agents)))
