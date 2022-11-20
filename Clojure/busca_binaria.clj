(ns untitled.core)

(defn busca-binaria
  ([lista numero]
   (loop [l 0 r (dec (count lista))]
     (let [mid (quot (+ r l) 2)]
       (if (>= r l)
         (cond
           (== (lista mid) numero) mid
           (>  (lista mid) numero) (recur l (dec mid))
           :else           (recur (inc mid) r))
         -1)))))


(defn -main []
  (let [lista (do (println "Insira uma lista: ")
                  (vec (map #(Integer/parseInt %) (clojure.string/split (clojure.string/trimr (read-line)) #" "))))
        numero (do (println "Buscar primeira ocorrência do número: ")
                   (Integer/parseInt (read-line)))
        indice (busca-binaria lista numero)]
    (cond
      (not= indice -1)
          (println (str "Primeira ocorrência encontrada no índice " indice "."))
      :else
          (println "Número não encontrado."))))

(-main)