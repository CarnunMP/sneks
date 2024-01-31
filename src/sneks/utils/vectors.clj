(ns sneks.utils.vectors)

(defn add [v1 v2 & vs]
  (assert (apply = (count v1) (count v2) (map count vs)))
  (apply mapv + v1 v2 vs))

(def dir->x+y
  {:n [0 -1]
   :s [0 1]
   :e [1 0]
   :w [-1 0]})

(comment
  (add [1 2] [3 4])
  (add [1 2] [3 4] [5 6])
  (add [1 2] [3 4] [5 6] [7])
  )
