(ns laba3-2.core
  (:gen-class))

(defn heavy [sleep f]
  (fn [& coll]
    (Thread/sleep sleep)
    (apply f coll)))

;; (defn my-filter [coll size_bloc pred-i]
;;   (let [pred (heavy 100 pred-i)]
;;     (apply concat
;;            (doall
;;             (map deref
;;                  (doall
;;                   (map #(future (doall (filter pred %))) (separation coll size_bloc))))))))


(def heavy-pred (heavy 100 even?))

(defn separation [coll size_bloc]
  (loop [acc [], new_coll coll]
    (if (>= size_bloc (count new_coll)) (conj acc (take size_bloc new_coll))
        (recur (conj acc (take size_bloc new_coll)) (drop size_bloc new_coll)))))

(defn my-filter-mac [size_bloc pred-i]
  (fn [coll]
    (let [pred (heavy 100 pred-i)]
      (->>
       (separation coll size_bloc)
       (map #(future (doall (filter pred %))))
       (doall)
       (map deref)
       (doall)
       (apply concat)))
    )
  )


(defn my-filter-batch [coll size_bloc size_batch_elem pred-i]
  (let [new_coll (separation coll size_batch_elem)
        my-filter (my-filter-mac size_bloc pred-i)]
    (apply concat(doall(map #(my-filter %) new_coll)))
    
   )
  )

(defn main []
  (time (my-filter-batch '(1 2 3 4 5 6 7 8 9 10) 2 4 even?))
  ;(time (my-filter-mac '(1 2 3 4 5 6 7 8 9 10) 2 even?))
  ;(time(doall(filter heavy-pred '(1 2 3 4 5 6))))
  )

(main)