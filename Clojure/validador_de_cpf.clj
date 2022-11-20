(ns untitled.core)

(defn verificar-digito
  [digito cpf]
  (let [digito-1 (dec digito)
        vetor-mult (flatten (conj (vec (range (+ 10 digito-1) 1 -1))
                                  (take (cond
                                          (= digito 1) 2
                                          (= digito 2) 1) (repeat 1))))]
    (->> cpf
         (mapv * vetor-mult)
         ((fn [cpf]
            (let [base (take (+ 9 digito-1) cpf)
                  resto (rem (* (reduce + base) 10) 11)]
              (= resto (cond
                         (= digito 1) (cpf 9)
                         (= digito 2) (cpf 10)))))))))

(defn validar-cpf [cpf]
  (->> cpf
       (re-seq #"[0-9]")
       (map #(Integer/parseInt %))
       ((fn [cpf]
          (and (= (count cpf) 11)
               (verificar-digito 1 cpf)
               (verificar-digito 2 cpf))))))

(defn -main []
  (println "Digite seu CPF: ")
  (let [cpf (validar-cpf (str (read-line)))]
    (cond
      (= cpf true) (println "CPF válido.")
      (= cpf false) (println "CPF inválido.")
      :else (println "."))))

(-main)
