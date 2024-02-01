(ns sneks.utils.logic)


;;; --- shared ---

(def empty-cell? (partial = \.))
(def sname-set #{\a \b \c})


;;; --- sneks, cells, levels ---

(defn snek-cell?
  "Returns true if cell contains a snek (snegment), else false."
  [cell]
  (and (string? cell)
       (sname-set (first cell))))

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

(defn get-layer-zero-cell [level [x y]]
  (get-in level [:layer/zero y x]))

(defn get-layer-one-cell [level [x y]]
  (get-in level [:layer/one y x]))

(defn get-snek [level sname]
  (get-in level [:sneks sname]))

(defn get-head [level sname]
  (get (get-snek level sname) 0))

(defn read-sneks
  "Returns snek maps for the layer."
  [layer-one]
  (loop [x 0, y 0
         sneks {}]
    (let [cell (get-in layer-one [y x])]
      (cond
        ;; end of row
        (= x (count (first layer-one)))
        (recur 0 (inc y) sneks)

        ;; end of cols — i.e. done
        (= y (count layer-one))
        sneks

        ;; assoc coord of snegment into sneks map
        (snek-cell? cell)
        (recur (inc x) y (assoc-in sneks [(first cell) (snek-cell->sneg-num cell)] [x y]))

        ;; next cell
        :else
        (recur (inc x) y sneks)))))
