(ns lisp_labs.lab2.lab1)

(defn make-integral-memoized [f h]
  (let [h (double (Math/abs (double h)))
        raw-integral
        (fn [x]
          (let [x    (double x)
                sign (if (>= x 0.0) 1.0 -1.0)
                x    (Math/abs x)
                k    (long (Math/floor (/ x h)))
                kh   (* k h)
                rem  (- x kh)

                xs   (map #(* % h) (range (inc k)))
                ys   (map (fn [t] (double (f t))) xs)

                areas    (map (fn [[y1 y2]]
                                (* 0.5 h (+ y1 y2)))
                              (partition 2 1 ys))
                base-sum (reduce + 0.0 areas)

                result (if (<= (Math/abs rem) 1.0e-15)
                         base-sum
                         (let [fk   (double (f kh))
                               fx   (double (f x))
                               area (* 0.5 rem (+ fk fx))]
                           (+ base-sum area)))]
            (* sign result)))]
    (memoize raw-integral)))

(defn -main [& args]
  (println "Численное интегрирование методом трапеций")
  (println "Функция: f(t) = t^2, шаг h = 0.001")

  (let [f  (fn [t] (* t t))
        h  0.001
        F  (make-integral-memoized f h)]

    (println "\nМемоизация")
    (let [start-total (System/nanoTime)]
      (doseq [x [1 2 3 4 5 6 7 5 6]]
        (let [start   (System/nanoTime)
              result  (F x)
              end     (System/nanoTime)
              elapsed (/ (- end start) 1e6)]
          (println (format "∫_0^%-2d t^2 dt ≈ %.6f  (%.3f мс)"
                           x result elapsed))))
      (let [end-total (System/nanoTime)
            total-ms  (/ (- end-total start-total) 1e6)]
        (println (format "\nОбщее время выполнения: %.3f мс" total-ms))))))
