(ns sneks.utils.vectors)

(defn add [v1 v2 & vs]
  (assert (apply = (count v1) (count v2) (map count vs)))
  (apply mapv + v1 v2 vs))

(comment
  (add [1 2] [3 4])
  (add [1 2] [3 4] [5 6])
  (add [1 2] [3 4] [5 6] [7])
  )
