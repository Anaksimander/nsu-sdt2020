(ns laba4.core
  (:gen-class))

(defn variable [name];перемеенная 
  {:pre [(keyword? name)]}
  (list ::var name))

(defn variable? [expr];проверяем переменная ли?
  (= (first expr) ::var))

(defn variable-name [v];получение значения
  [v] (second v))

(defn same-variables? [v1 v2];сравниваем переменные
  (and (variable? v1) (variable? v2)
       (= (variable-name v1) (variable-name v2))))

(defn constant [num];константа
  {:pre [(boolean? num)]}
  (list ::const num))

(defn constant? [expr];константа?
  (= (first expr) ::const))

(defn constant-value [expr];возвращает значение константы
  (second expr))

(defn log_and [expr & rest]
  (cons ::and (cons expr rest)))

(defn log_or [expr & rest]
  (cons ::or (cons expr rest)))

(defn log_neg [expr]
  (cons ::neg expr))

(defn log_and? [expr]
  (= (first expr) ::and))

(defn log_or? [expr]
  (= (first expr) ::or))

(defn log_neg? [expr]
  (= (first expr) ::neg))

(defn bas_oper? [expr]
  (or (log_neg? expr) (log_or? expr) (log_and? expr)))

(declare calculate)

(defn and_calculate [expr vr newvr]
  (let [res (map #(calculate % vr newvr) (rest expr))]            ;погружение в глубь
    (if (every? #(constant? %) res)                               ;если все константы то true
      (constant (every? true? (map constant-value res)))          ;если есть хотя бы один 0 то false иначе true
      (if (some #(and (constant? %) (not (constant-value %))) res);если есть хотяба одна константа 0 то false
        (constant false)
        (log_and res)))))


(defn or_calculate [expr vr newvr]
  (let [res (map #(calculate % vr newvr) (rest expr))]          ;погружение в глубь
    (if (every? #(constant? %) res)                             ;если все константы то true
      (constant (boolean (some true? (map constant-value res))));если есть хотя бы один 1 то true иначе false
      (if (some #(and (constant? %) (constant-value %)) res)    ;если есть хотяба одна константа 1 то true
        (constant true)
        (log_or res)))))

(defn neg_calculate [expr vr newvr]
  (let [res (calculate (rest expr) vr newvr)];погружение в глубь
    (if (constant? res)                      ;если это константа то инкрементируем ее
      (constant (not (constant-value res)))
      (log_neg res))))

(def calculate_rule
  (atom
   (list
    [(fn [expr vr new_vr]
       (constant? expr))
     (fn [expr vr new_vr] expr)] ;константа

    [(fn [expr vr new_vr] ;подмена переменной
       (and (variable? expr) (same-variables? expr vr)))
     (fn [expr vr new_vr] new_vr)] ;переменная

    [(fn [expr vr new_vr] ;переменная
       (variable? expr))
     (fn [expr vr new_vr] expr)] ; ничего не меняем

    [(fn [expr vr new_vr] (log_neg? expr)) ;базовые операции
     neg_calculate]

    [(fn [expr vr newvr] (log_or? expr))
     or_calculate]

    [(fn [expr vr newvr] (log_and? expr))
     and_calculate])))

(defn calculate [expr vr new_vr] ;vr <- new_vr подстановка значения переменной в выражение
  ((some
    (fn [rule]
      (if ((first rule) expr vr new_vr)
        (second rule)
        false))
    @calculate_rule)
   expr vr new_vr))

(defn to_simple [expr];упрощение
  (cond
    (log_or? expr)
    (apply log_or (reduce #(if (log_or? %2) (concat %1 (rest %2)) (cons %2 %1)) (cons '() (distinct (map to_simple (rest expr))))))

    (log_and? expr)
    (apply log_and (reduce #(if (log_and? %2) (concat %1 (rest %2)) (cons %2 %1)) (cons '() (distinct (map to_simple (rest expr))))))
    :else expr))

(defn exch [expr1 expr2]
  (if (log_or? expr1)
    (if (log_or? expr2)
      (for [x (rest expr1) y (rest expr2)] (log_and x y))
      (for [x (rest expr1)] (log_and x expr2)))
    (if (log_or? expr2)
      (for [x (rest expr2)] (log_and expr1 x))
      (log_and expr1 expr2))))

(defn distib [expr]
  (cond
    (log_or? expr) (apply log_or (map distib (rest expr)))
    (log_and? expr)
    (let [args (map distib (rest expr))]
      (reduce #(apply log_or (exch %1 %2)) args))
    :else expr))

(declare convert_b)

(def convert_rule ;
  (atom
   (list
    [(fn [expr] (constant? expr))
     (fn [expr] expr)] ;если константа то вернём её

    [(fn [expr] (variable? expr))
     (fn [expr] expr)] ;если переменная то вернём её

    [(fn [expr] (log_neg? expr))
     (fn [expr] (log_neg (convert_b (rest expr))))] 

    [(fn [expr] (log_or? expr))
     (fn [expr] (apply log_or (map #(convert_b %) (rest expr))))] 

    [(fn [expr] (log_and? expr))
     (fn [expr] (apply log_and (map #(convert_b %) (rest expr))))] 
    )))

(defn convert_b [expr] 
  ((some
    (fn [rule]
      (if ((first rule) expr)
        (second rule)
        false))
    @convert_rule)
   expr))



(defn negat_miss [expr]
  (if (bas_oper? expr) ;провека на операцию иначе возвращаем перем\конст
    (if (log_neg? expr)
      (let [res (rest expr)]
        (cond
          (log_neg? res) (negat_miss (rest res));отбрасываем 
          (log_or? res) (apply log_and (map #(negat_miss (log_neg %)) (rest res)));убираем нег и преоброзование
          (log_and? res) (apply log_or (map #(negat_miss (log_neg %)) (rest res)))
          :else (log_neg res)));если все мимо то возвращаем нег
      (cons (first expr) (map #(negat_miss %) (rest expr))));продолжаем поиск в глубину
    expr))




(defn convert_dnf [expr];преобразование выражение expr в дизьюнктивную нормальную форму
  (->> expr
       (convert_b) 
       (negat_miss)
       (to_simple) 
       (distib) 
       (to_simple) 
       ))


(convert_dnf
 (log_and (log_or (variable :x) (variable :y)) (log_or (variable :x) (variable :y))))

(convert_dnf
 (log_neg (log_or (log_or (log_neg (variable :x)) (variable :y)) (log_neg (log_or (log_neg (variable :y)) (variable :z))))))

(calculate
 (variable :x)
 (variable :x);какую переменную заменим 
 (variable :a))
