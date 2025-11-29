(ns lisp-labs.lab4.bool-test
  (:require [clojure.test :refer :all]
            [lisp-labs.lab4.bool :as b]))

;; -------------------------------
;; Тест 1. Подстановка переменной
;; -------------------------------

(deftest subst-basic-test
  (testing "Подстановка переменной в простое выражение"
    ;; (p ∧ q), подставляем p = true
    (let [p    (b/var 'p)
          q    (b/var 'q)
          expr (b/and p q)
          expr' (b/subst expr 'p true)]
      ;; исходное при p=true и результат expr' ведут себя одинаково
      (doseq [qv [false true]]
        (is (= (b/eval-expr expr  {'p true  'q qv})
               (b/eval-expr expr' {'q qv})))))
    ;; (p → q), подставляем p = false
    (let [p    (b/var 'p)
          q    (b/var 'q)
          expr (b/implies p q)
          expr' (b/subst expr 'p false)]
      ;; исходное при p=false и результат expr' должны быть эквивалентны
      (doseq [qv [false true]]
        (is (= (b/eval-expr expr  {'p false 'q qv})
               (b/eval-expr expr' {'q qv})))))))

;; ---------------------------------
;; Тест 2. Простые случаи для ДНФ
;; ---------------------------------

(deftest to-dnf-simple-test
  (testing "Переменная, константа и отрицание уже в ДНФ"
    (let [x (b/var 'x)]
      ;; переменная
      (is (= x (b/to-dnf x)))
      ;; константы
      (is (= true  (b/to-dnf true)))
      (is (= false (b/to-dnf false)))
      ;; отрицание переменной
      (is (= (b/not x) (b/to-dnf (b/not x)))))))

;; ---------------------------------
;; Тест 3. Импликация → ДНФ
;; ---------------------------------

(deftest to-dnf-implies-test
  (testing "Элиминация импликации p → q = ¬p ∨ q"
    (let [p    (b/var 'p)
          q    (b/var 'q)
          expr (b/implies p q)
          dnf  (b/to-dnf expr)]
      ;; проверяем по таблице истинности
      (doseq [pv [false true]
              qv [false true]]
        (let [env {'p pv 'q qv}]
          (is (= (b/eval-expr expr env)
                 (b/eval-expr dnf  env))))))))

;; ---------------------------------
;; Тест 4. Распределение конъюнкции
;; ---------------------------------

(deftest to-dnf-distribute-test
  (testing "x ∧ (y ∨ z) → (x ∧ y) ∨ (x ∧ z)"
    (let [x (b/var 'x)
          y (b/var 'y)
          z (b/var 'z)
          expr (b/and x (b/or y z))
          dnf  (b/to-dnf expr)
          expected (b/or (b/and x y)
                         (b/and x z))]
      (doseq [xv [false true]
              yv [false true]
              zv [false true]]
        (let [env {'x xv 'y yv 'z zv}]
          (is (= (b/eval-expr expected env)
                 (b/eval-expr dnf      env))))))))

;; ---------------------------------
;; Тест 5. Сложное выражение
;; ---------------------------------

(deftest to-dnf-complex-test
  (testing "Сложное выражение с импликацией и скобками"
    (let [a  (b/var 'a)
          b1 (b/var 'b)  ;; чтобы не путать с alias'ом b
          c  (b/var 'c)
          d  (b/var 'd)
          ;; (a ∧ b) → (c ∨ ¬d)
          expr (b/implies (b/and a b1)
                          (b/or c (b/not d)))
          dnf  (b/to-dnf expr)]
      (doseq [av [false true]
              bv [false true]
              cv [false true]
              dv [false true]]
        (let [env {'a av 'b bv 'c cv 'd dv}]
          (is (= (b/eval-expr expr env)
                 (b/eval-expr dnf  env))))))))

;; ---------------------------------
;; Тест 6. Законы де Моргана через to-dnf
;; ---------------------------------

(deftest demorgan-test
  (testing "to-dnf реализует законы де Моргана"
    (let [x (b/var 'x)
          y (b/var 'y)]
      ;; ¬(x ∧ y) ≡ ¬x ∨ ¬y
      (let [expr1 (b/not (b/and x y))
            expr2 (b/or (b/not x) (b/not y))]
        (doseq [xv [false true]
                yv [false true]]
          (let [env {'x xv 'y yv}]
            (is (= (b/eval-expr (b/to-dnf expr1) env)
                   (b/eval-expr (b/to-dnf expr2) env))))))
      ;; ¬(x ∨ y) ≡ ¬x ∧ ¬y
      (let [expr1 (b/not (b/or x y))
            expr2 (b/and (b/not x) (b/not y))]
        (doseq [xv [false true]
                yv [false true]]
          (let [env {'x xv 'y yv}]
            (is (= (b/eval-expr (b/to-dnf expr1) env)
                   (b/eval-expr (b/to-dnf expr2) env)))))))))

;; ---------------------------------
;; Тест 7. XOR как встроенная операция
;; ---------------------------------

(deftest xor-dnf-test
  (testing "Исключающее ИЛИ как встроенная операция"
    (let [x (b/var 'x)
          y (b/var 'y)
          expr (b/xor x y)
          dnf  (b/to-dnf expr)]
      (doseq [xv [false true]
              yv [false true]]
        (let [env {'x xv 'y yv}]
          (is (= (b/eval-expr expr env)
                 (b/eval-expr dnf  env)))
          (is (= (or (and xv (not yv))
                     (and (not xv) yv))
                 (b/eval-expr expr env))))))))

;; ---------------------------------
;; Тест 8. Стрелка Пирса (NOR) как встроенная операция
;; ---------------------------------

(deftest nor-dnf-test
  (testing "Стрелка Пирса (NOR) как встроенная операция"
    (let [x (b/var 'x)
          y (b/var 'y)
          expr     (b/nor x y)               ;; встроенный NOR
          expected (b/and (b/not x) (b/not y)) ;; по де Моргану: ¬x ∧ ¬y
          dnf      (b/to-dnf expr)]
      (doseq [xv [false true]
              yv [false true]]
        (let [env {'x xv 'y yv}]
          ;; 1) DNF не ломает семантику NOR
          (is (= (b/eval-expr expr env)
                 (b/eval-expr dnf  env)))
          ;; 2) NOR эквивалентен ¬x ∧ ¬y
          (is (= (b/eval-expr expr env)
                 (b/eval-expr expected env)))
          ;; 3) Проверяем семантику NOR
          (is (= (not (or xv yv))
                 (b/eval-expr expr env))))))))
