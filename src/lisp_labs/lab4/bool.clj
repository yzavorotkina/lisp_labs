(ns lisp-labs.lab4.bool)

(ns lisp-labs.lab4.bool
  (:refer-clojure :exclude [and or not])
  )

;; ------------------------------
;; Базовое представление
;; ------------------------------
;;
;; Константы: true / false (обычные Clojure-boolean'ы)
;; Переменные: символы, например 'p, 'q, 'x
;;
;; Составные выражения:
;;   {:op :and     :args [e1 e2 ...]}
;;   {:op :or      :args [e1 e2 ...]}
;;   {:op :not     :arg  e}
;;   {:op :implies :args [e1 e2]}
;;   {:op :xor     :args [e1 e2]}
;;   {:op :nor     :args [e1 e2]}

(defn bool-const?
  "Истина, если expr — булева константа true/false."
  [expr]
  (or (true? expr) (false? expr)))

(defn bool-var?
  "Истина, если expr — переменная (символ)."
  [expr]
  (symbol? expr))

(defn op-node?
  "Истина, если expr — карта с ключом :op (узел операции)."
  [expr]
  (and (map? expr) (contains? expr :op)))

(defn op-of
  "Вернуть тип операции (:and, :or, :not, :implies, ...)."
  [expr]
  (:op expr))

(defn args-of [expr] (:args expr))
(defn arg-of  [expr] (:arg expr))

(defn- flatten-same-op
  "Плоская развёртка вложенных однотипных операций:
   (and a (and b c)) → (and a b c)."
  [op exprs]
  (mapcat (fn [e]
            (if (and (op-node? e) (= op (op-of e)))
              (args-of e)
              [e]))
          exprs))

(defn var
  "Создаёт переменную (на самом деле просто возвращает символ).
   Пример: (var 'p) → 'p"
  [name]
  (assert (symbol? name) "Имя переменной должно быть символом")
  name)

(defn const
  "Создаёт булеву константу (true/false)."
  [v]
  (boolean v))

(defn not
  "Отрицание булева выражения.
   Упрощает константы и двойное отрицание."
  [e]
  (cond
    (true? e)  false
    (false? e) true
    (and (op-node? e) (= :not (op-of e))) (arg-of e) ;; ¬(¬e) = e
    :else {:op :not :arg e}))

(defn and
  "Конъюнкция (логическое И).
   - (and)     = true
   - (and a)   = a
   - (and a b) = a ∧ b
   Плоско разворачивает вложенные :and и упрощает константы."
  [& es]
  (let [flat (flatten-same-op :and es)]
    (cond
      (some false? flat) false
      :else
      (let [no-true (remove true? flat)]
        (cond
          (empty? no-true) true
          (= 1 (count no-true)) (first no-true)
          :else {:op :and :args (vec no-true)})))))

(defn or
  "Дизъюнкция (логическое ИЛИ).
   - (or)     = false
   - (or a)   = a
   - (or a b) = a ∨ b
   Плоско разворачивает вложенные :or и упрощает константы."
  [& es]
  (let [flat (flatten-same-op :or es)]
    (cond
      (some true? flat) true
      :else
      (let [no-false (remove false? flat)]
        (cond
          (empty? no-false) false
          (= 1 (count no-false)) (first no-false)
          :else {:op :or :args (vec no-false)})))))

(defn implies
  "Импликация (p → q).
   Для ДНФ будет разворачиваться в (or (not p) q)."
  [a b]
  {:op :implies :args [a b]})

(defn xor
  "Исключающее ИЛИ (p ⊕ q)."
  [a b]
  {:op :xor :args [a b]})

(defn nor
  "Стрелка Пирса (NOR): p ↓ q = ¬(p ∨ q)."
  [a b]
  {:op :nor :args [a b]})

(defn eval-expr
  "Вычисляет булево выражение expr в окружении env.
   env — мапа {переменная → значение}, например {'p true, 'q false}.

   Пример:
   (eval-expr (and (var 'p) (not (var 'q)))
              {'p true 'q false}) ;=> true"
  [expr env]
  (cond
    (bool-const? expr) expr
    (bool-var? expr)   (boolean (get env expr false))

    (op-node? expr)
    (case (op-of expr)
      :not     (clojure.core/not (eval-expr (arg-of expr) env))
      :and     (every? true? (map #(eval-expr % env) (args-of expr)))
      :or      (boolean (some true? (map #(eval-expr % env) (args-of expr))))
      :implies (let [[a b] (args-of expr)]
                 (or (clojure.core/not (eval-expr a env))
                     (eval-expr b env)))
      :xor     (let [[a b] (args-of expr)
                     a-val (eval-expr a env)
                     b-val (eval-expr b env)]
                 ;; XOR: (a AND NOT b) OR (NOT a AND b)
                 (or (and a-val (not b-val))
                     (and (not a-val) b-val)))
      :nor     (let [[a b] (args-of expr)
                     a-val (eval-expr a env)
                     b-val (eval-expr b env)]
                 ;; NOR: NOT (a OR b)
                 (not (or a-val b-val)))
      (throw (ex-info (str "Неизвестная операция: " (op-of expr))
                      {:expr expr})))

    :else (throw (ex-info "Некорректное выражение" {:expr expr}))))

(defn- literal?
  "Литерал = переменная или её отрицание (или константа true/false)."
  [e]
  (or (bool-const? e)
      (bool-var? e)
      (and (op-node? e)
           (= :not (op-of e))
           (bool-var? (arg-of e)))))

(defn eliminate-implies
  "Убирает импликации: (p → q) = (¬p ∨ q)."
  [expr]
  (cond
    (bool-const? expr) expr
    (bool-var? expr)   expr

    (op-node? expr)
    (case (op-of expr)
      :implies (let [[a b] (args-of expr)]
                 (or (not (eliminate-implies a))
                     (eliminate-implies b)))
      :xor     (let [[a b] (args-of expr)]
                 (or (and (eliminate-implies a) (not (eliminate-implies b)))
                     (and (not (eliminate-implies a)) (eliminate-implies b))))
      :nor     (let [[a b] (args-of expr)]
                 (not (or (eliminate-implies a) (eliminate-implies b))))
      :not     (not (eliminate-implies (arg-of expr)))
      :and     (apply and (map eliminate-implies (args-of expr)))
      :or      (apply or  (map eliminate-implies (args-of expr)))
      (throw (ex-info "Неизвестная операция при eliminate-implies"
                      {:expr expr})))

    :else (throw (ex-info "Некорректное выражение" {:expr expr}))))

(defn- nnf*
  "Вспомогательная функция для приведения к NNF.
   Параметр neg? означает, что к выражению уже применено отрицание."
  [expr neg?]
  (cond
    (bool-const? expr)
    (if neg? (clojure.core/not expr) expr)

    (bool-var? expr)
    (if neg?
      (not expr) ;; литерал ¬p
      expr)

    (op-node? expr)
    (case (op-of expr)
      :not (nnf* (arg-of expr) (not neg?))

      :and (if neg?
             ;; ¬(a ∧ b) = ¬a ∨ ¬b
             (apply or  (map #(nnf* % true) (args-of expr)))
             (apply and (map #(nnf* % false) (args-of expr))))

      :or  (if neg?
             ;; ¬(a ∨ b) = ¬a ∧ ¬b
             (apply and (map #(nnf* % true) (args-of expr)))
             (apply or  (map #(nnf* % false) (args-of expr))))

      (throw (ex-info "Неизвестная операция при nnf*"
                      {:expr expr :neg? neg?})))

    :else (throw (ex-info "Некорректное выражение" {:expr expr}))))

(defn to-nnf
  "Приводит выражение к Нормальной форме по отрицаниям (NNF).
   На выходе нет импликаций, отрицание стоит только перед переменными."
  [expr]
  (-> expr
      eliminate-implies
      (nnf* false)))

(defn- nnf->clauses
  "Преобразует выражение в NNF в множество клауз (список векторов литералов)."
  [expr]
  (cond
    (true? expr)       [[]]
    (false? expr)      []
    (literal? expr)    [[expr]]

    (op-node? expr)
    (case (op-of expr)
      :or
      (->> (args-of expr)
           (map nnf->clauses)
           (apply concat))

      :and
      (let [sub (map nnf->clauses (args-of expr))]
        (reduce
          (fn [acc cls]
            (for [a acc
                  c cls]
              (vec (concat a c))))
          [[]]
          sub))

      (throw (ex-info "Ожидались только :and или :or в NNF"
                      {:expr expr})))

    :else (throw (ex-info "Некорректное выражение в nnf->clauses"
                          {:expr expr}))))

(defn- clauses->dnf
  "Собирает AST выражения (в нашей структуре) из списка клауз."
  [clauses]
  (cond
    (empty? clauses) false
    (some empty? clauses) true

    :else
    (let [terms (map (fn [clause]
                       (case (count clause)
                         0  true
                         1  (first clause)
                         (apply and clause)))
                     clauses)]
      (case (count terms)
        0 false
        1 (first terms)
        (apply or terms)))))

(defn to-dnf
  "Приводит выражение к Дизъюнктивной нормальной форме (ДНФ).
   Алгоритм:
    1) убираем импликации и преобразуем специальные операции;
    2) приводим к NNF;
    3) строим набор клауз (список конъюнктов);
    4) собираем AST в виде OR-над-AND-ами литералов."
  [expr]
  (-> expr
      to-nnf
      nnf->clauses
      clauses->dnf))

(defn subst
  "Подстановка значения переменной var-name в выражение expr
   c последующим приведением к ДНФ.
   var-name — символ, value — boolean.

   Возвращает новое выражение (в ДНФ).

   Пример:
   (subst (and (var 'p) (or (var 'q) (not (var 'p))))
          'p true)
   ⇒ ДНФ-выражение, эквивалентное (or q false) → q."
  [expr var-name value]
  (letfn [(subst* [e]
            (cond
              (bool-const? e) e
              (bool-var? e)   (if (= e var-name) (boolean value) e)

              (op-node? e)
              (case (op-of e)
                :not     (not (subst* (arg-of e)))
                :and     (apply and (map subst* (args-of e)))
                :or      (apply or  (map subst* (args-of e)))
                :implies (implies (subst* (first (args-of e)))
                                  (subst* (second (args-of e))))
                :xor     (xor (subst* (first (args-of e)))
                              (subst* (second (args-of e))))
                :nor     (nor (subst* (first (args-of e)))
                              (subst* (second (args-of e))))
                (throw (ex-info "Неизвестная операция при subst*"
                                {:expr e})))

              :else (throw (ex-info "Некорректное выражение при subst*"
                                    {:expr e}))))]

    (-> (subst* expr)
        to-dnf)))