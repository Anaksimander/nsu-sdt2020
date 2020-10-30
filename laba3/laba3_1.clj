(ns laba3-1.core
  (:gen-class))

(defn heavy [sleep f]
  (fn [& coll]
    (Thread/sleep sleep)
    (apply f coll)
    ))

(def heavy-pred (heavy 100 even?))
 
(defn separation [coll size_bloc]
  (loop [acc [], new_coll coll]
    (if (>= size_bloc (count new_coll)) (conj acc (take size_bloc new_coll))
      (recur (conj acc (take size_bloc new_coll)) (drop size_bloc new_coll))
    )
    ))

(defn my-filter [coll size_bloc]
  apply concat
  (doall
   (map deref
        (doall
         (map #(future (doall (filter heavy-pred %))) (separation coll size_bloc) )
        ))))  

(defn main []
  (time(my-filter '(1 2 3 4 5 6) 2))
  (time(doall(filter heavy-pred '(1 2 3 4 5 6))))
  )

(main)

;(time (my-filter '(1 2 3 4 5 6) 2))