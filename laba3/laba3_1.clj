(ns laba3-1.core
  (:gen-class))

(defn heavy [sleep f]
  (fn [& coll]
    (Thread/sleep sleep)
    (apply f coll)
    ))

;(def heavy-pred (heavy 100 even?))
 
(defn separation [coll size_bloc]
  (loop [acc [], new_coll coll]
    (if (>= size_bloc (count new_coll)) (conj acc (take size_bloc new_coll))
      (recur (conj acc (take size_bloc new_coll)) (drop size_bloc new_coll))
    )
    ))

(defn my-filter [coll size_bloc pred]
  (apply concat
  (doall
   (map deref
        (doall
         (map #(future (doall (filter (heavy 100 pred) %))) (separation coll size_bloc) )
        )))))  

(defn my-filter-mac [coll size_bloc pred]
  (->>
   (separation coll size_bloc)
   (map #(future (doall (filter (heavy 100 pred) %))))
   (doall)
   (map deref)
   (doall)
   (apply concat)))

(defn main []
  ;(time(my-filter '(1 2 3 4 5 6) 2 even?))
  ;(time(doall(filter heavy-pred '(1 2 3 4 5 6))))
  (time (my-filter-mac (take 1000 (range)) 2 even?))
  )



(main)