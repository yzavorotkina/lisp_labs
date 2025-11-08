(ns lisp-labs.lab2.lab2)

(defn make-integral-lazy [f h]
  (let [h        (double (Math/abs (double h)))
        xs       (iterate #(+ % h) 0.0)
        fs       (map (fn [x] (double (f x))) xs)
        areas    (map (fn [[a b]] (* 0.5 h (+ a b)))
                      (partition 2 1 fs))
        partials (reductions + 0.0 areas)]
    (fn [x]
      (let [x     (double x)
            sign  (if (>= x 0.0) 1.0 -1.0)
            x     (Math/abs x)
            k     (long (Math/floor (/ x h)))
            kh    (* k h)
            rem   (- x kh)]
        (if (<= rem 1.0e-15)
          (* sign (nth partials k))
          (let [Fk   (nth partials k)
                fk   (nth fs k)
                fx   (double (f x))
                area (* 0.5 rem (+ fk fx))]
            (* sign (+ Fk area))))))))

(defn -main [& _]
  (println "Ленивая версия: метод трапеций + префиксные суммы")
  (let [f (fn [t] (* t t))
        h 0.001
        F (make-integral-lazy f h)
        start (System/nanoTime)]
    (doseq [x [1 2 3 4 5 -2 -3 2.5 3.7]]
      (let [t0 (System/nanoTime)
            v  (F x)
            t1 (System/nanoTime)]
        (println (format "∫_0^%s t^2 dt ≈ %.6f  (%.3f мс)"
                         x v (/ (- t1 t0) 1e6)))))
    (let [total-ms (/ (- (System/nanoTime) start) 1e6)]
      (println (format "\nОбщее время: %.3f мс" total-ms)))))

