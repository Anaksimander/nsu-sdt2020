(ns laba3-1.core
  (:gen-class))

(defn heavy-pred [sleep f]
  (Thread/sleep sleep)
  f)
 
(defn separation [coll size_bloc]
  (loop [acc [], new_coll coll]
    (if (>= size_bloc (count new_coll)) (conj acc (take size_bloc new_coll))
      (recur (conj acc (take size_bloc new_coll)) (drop size_bloc new_coll) )
    )
    )
)

(defn my-filter [coll size_bloc sleep pred]
  (apply concat
  (doall
   (map deref
        (doall
         (map #(future (filter (heavy-pred sleep pred) %)) (separation coll size_bloc)))))))


(defn main []
  
  )

(time (my-filter '(1 2 3 4 5 6) 2 100 even?))