(ns laba3-2.core
  (:gen-class))

(defn heavy [sleep f]
  (fn [& coll]
    (Thread/sleep sleep)
    (apply f coll)))

(def heavy-pred (heavy 100 even?))

(defn separation [size_bloc coll]
  (lazy-seq
   (when-let [new_coll (seq coll)]
     (let [bloc (doall (take size_bloc new_coll))]
       (cons bloc (separation size_bloc (drop size_bloc coll)))))))


(defn my-filter-mac [ pred]
  (fn [coll]
    (->>
      coll
     (map #(future (doall(filter pred %))))
     (doall)
     (map deref)
     (apply concat)
     (lazy-seq))))


(defn my-filter-batch [size_chank size_batch pred coll]
     (->>
      (separation size_chank coll)
      (separation size_batch) 
      (map #((my-filter-mac pred) %))
      (apply concat)
      ;(lazy-cat)
      ))

(defn main []
  (let [f (my-filter-batch 2 2 heavy-pred (range))]
    (time (take 5 f))
    (time (doall(take 5 f)))
    )
  )

