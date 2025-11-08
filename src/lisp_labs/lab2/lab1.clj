(ns lisp-labs.lab2.lab1)

(defn make-integral-memoized [f h]
  (let [h (double (Math/abs (double h)))
        cache (atom {0.0 0.0})]

    (fn [x]
      (let [x (double x)
            forward? (>= x 0.0)
            a (if forward? 0.0 x)
            b (if forward? x 0.0)
            dir (if forward? 1.0 -1.0)]

        (let [known (keys @cache)
              closest (if forward?
                        (apply max (filter #(<= % b) known))
                        (apply min (filter #(>= % b) known)))
              Fstart (get @cache closest)
              start closest]

          (loop [t start, acc Fstart]
            (let [remain (Math/abs (- b t))]
              (if (<= remain 1.0e-15)
                (do
                  (swap! cache assoc x acc)
                  (if forward? acc (- acc)))
                (let [len (min h remain)
                      t2  (+ t (* dir len))
                      ft  (double (f t))
                      ft2 (double (f t2))
                      area (* 0.5 len (+ ft ft2))
                      new-acc (+ acc area)]
                  (swap! cache assoc t2 new-acc)
                  (recur t2 new-acc))))))))))
(defn -main [& args]
  (println "Численное интегрирование методом трапеций")
  (println "Функция: f(t) = t^2, шаг h = 0.001")

  (let [f  (fn [t] (* t t))
        h  0.001
        F  (make-integral-memoized f h)
        start-total (System/nanoTime)]

    (doseq [x [1 2 3 4 5]]
      (let [start (System/nanoTime)
            result (F x)
            end (System/nanoTime)
            elapsed (/ (- end start) 1e6)]
        (println (format "∫_0^%-2d t^2 dt ≈ %.6f  (%.3f мс)"
                         x result elapsed))))

    (let [end-total (System/nanoTime)
          total-ms (/ (- end-total start-total) 1e6)]
      (println (format "\nОбщее время выполнения: %.3f мс" total-ms)))))
