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

(defn my-filter [coll size_bloc pred-i]
  (let [pred (heavy 100 pred-i)]
  (apply concat
  (doall
   (map deref
        (doall
         (map #(future (doall (filter pred %))) (separation coll size_bloc) )
        ))))))  

(defn my-filter-mac [coll size_bloc pred-i]
  (let [pred (heavy 100 pred-i)]
  (->>
   (separation coll size_bloc)
   (map #(future (doall (filter pred %))))
   (doall)
   (map deref)
   (doall)
   (apply concat))))


(defn main []
  (time(my-filter '(1 2 3 4 5 6 7 8 9 10) 5 even?))
  ;(time(doall(filter heavy-pred '(1 2 3 4 5 6))))
  ;(time (my-filter-mac '(1 2 3 4 5 6) 2 even?))
  ;(time (my-filter (take 1000 (range)) 2 even?))
  )

(main)