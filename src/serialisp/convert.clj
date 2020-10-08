(ns serialisp.convert)

(defn- const-val? [v]
  (or (number? v) (string? v) (symbol? v) (keyword? v)))

(defn- cons* [car & tail]
  (if (empty? tail)
    car
    (cons car (apply cons* tail))))

(defn- vons* [& args]
  (vec (apply cons* args)))

(declare cps*)

(defn sym-number->sym [number]
  (str "continuation" number))

(defn- cps-funcall [prevs expression
                    {:keys [nexts sym-numbers cont]
                     :or {nexts nil} :as meta-data}]
  (cond
    (and (empty? expression)
         (empty? nexts)) (vec (cons* (first prevs) cont (next prevs)))
    (empty? expression)
    (let [current-expression (first nexts)]
      (vons* (first prevs)
             ["lambda" [(sym-number->sym (:sym-number current-expression))]
              (cps* (:expression-type current-expression)
                    (conj (:prevs current-expression)
                          (sym-number->sym (:sym-number current-expression)))
                    (:expression current-expression)
                    (assoc meta-data
                           :nexts (next nexts)
                           :sym-numbers (next sym-numbers)))]
             (next prevs)))
    :else
    (cps* nil nil (first expression)
          (assoc meta-data
                 :nexts (cons {:sym-number (first sym-numbers)
                               :expression-type :funcall
                               :prevs prevs
                               :expression (next expression)}
                              nexts)
                 :sym-numbers (next sym-numbers)))))

(defn cps-if [prevs expression
              {:keys [sym-numbers nexts] :as meta-data}]
  (if (= (count prevs) 1)
    (let [true-expression (cps* nil nil (first expression) meta-data)
          false-expression (cps* nil nil (second expression) meta-data)]
      ["if" (first prevs) true-expression false-expression])
    (cps* nil nil (second expression)
          (assoc meta-data
                 :nexts (cons {:sym-number (first sym-numbers)
                               :expression-type :if
                               :prevs prevs
                               :expression (nnext expression)}
                              nexts)
                 :sym-numbers (next sym-numbers)))))

(defn cps-const [v {:keys [nexts sym-numbers] :or {nexts nil} :as meta-data}]
  (if (empty? nexts)
    v
    (let [current-expression (first nexts)]
      (cps* (:expression-type current-expression)
            (conj (:prevs current-expression) v)
            (:expression current-expression)
            (assoc meta-data
                   :nexts (next nexts)
                   :sym-numbers (cons (:sym-number current-expression)
                                      sym-numbers))))))

(defn cps-lambda [[_ bindings body] {:keys [sym-numbers] :as meta-data}]
  (let [cont-sym (sym-number->sym (first sym-numbers))]
    ["lambda" (vec (cons cont-sym bindings))
     (cps* nil nil body (assoc meta-data :sym-numbers (next sym-numbers)
                               :cont cont-sym))]))

(defn cps* [exp-type prev-expressions expression meta-data]
  (if (const-val? expression)
    (cps-const expression meta-data)
    (case exp-type
      :if (cps-if prev-expressions expression meta-data)
      :funcall (cps-funcall prev-expressions expression meta-data)
      (case (first expression)
        "lambda" (cps-lambda expression meta-data)
        "if" (cps* :if [] expression meta-data)
        (cps* :funcall [] expression meta-data)))))

(defn cps [expression]
  (cps* nil nil expression {:sym-numbers (range)
                            :cont nil}))
