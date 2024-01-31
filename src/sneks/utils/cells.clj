(ns sneks.utils.cells
  (:require [sneks.utils.sneks :as us]
            [sneks.levels.levels :as levels]))

(def empty-cell? (partial = \.))

(defn snek-cell?
  "Returns true if cell contains a snek (snegment), else false."
  [cell]
  (and (string? cell)
       (us/sname-set (first cell))))

(defn this-snek-cell?
  "Returns true if cell contains a snegment of *this* snek."
  [sname cell]
  (and (string? cell)
       (= sname (first cell))))

(defn snek-cell->sneg-num
  "Returns the number of the snegment."
  [cell]
  {:pre [(snek-cell? cell)]}
  (parse-long (subs cell 1)))

(defn get-cell [layer [x y]]
  (get-in @levels/level* [layer y x]))
