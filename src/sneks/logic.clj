(ns sneks.logic
  (:require
    [clojure.set :as set]
    [sneks.levels.levels :as levels]
    [sneks.utils.vectors :as uv]))


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

(defn get-layer-zero-cell [[x y]]
  (get-in @levels/level* [:layer/zero y x]))

(defn get-layer-one-cell [[x y]]
  (get-in @levels/level* [:layer/one y x]))

(defn get-snek [sname]
  (get-in @levels/level* [:sneks sname]))

(defn get-head [sname]
  (get (get-snek sname) 0))

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

(defn new-head
  "If the snamed snek can move in the given dir, returns its new head coord. Else nil."
  [sname dir]
  (let [adj-coord (uv/add (get-head sname) (uv/dir->x+y dir))
        adj-cell (get-layer-one-cell adj-coord)]
    (when (empty-cell? adj-cell)
      adj-coord)))

(defn move-snek!
  "Moves the snamed snek in the given dir(ection) if it can move, returning the new value of level*. Else nil."
  [sname dir]
  {:pre [(#{:n :e :s :w} dir)]}

  ;; only move if _can_ move
  (when-let [new-head (new-head sname dir)]
    (let [level @levels/level*
          snek (get-snek sname)
          snek' (merge {0 new-head}
                       ;; for the rest of the body, move snegments along
                       (reduce (fn [m i]
                                 (assoc m i (get snek (dec i))))
                               {} (range 1 (count snek))))
          layer-one' (loop [x 0, y 0
                            layer (:layer/one level)]
                       (let [cell (get-in layer [y x])
                             sneg-num' (get (set/map-invert snek') [x y])
                             cell' (cond
                                     ;; new snegment number for this cell -> write snegment
                                     sneg-num' (str sname sneg-num')
                                     ;; was a snek cell -> now empty
                                     (this-snek-cell? sname cell) \.
                                     ;; otherwise, no change
                                     :else cell)]
                         (cond
                           ;; end of row
                           (= x (count (first layer)))
                           (recur 0 (inc y) layer)

                           ;; end of cols — i.e. done
                           (= y (count layer))
                           layer

                           ;; write cell and recur
                           :else
                           (recur (inc x) y (assoc-in layer [y x] cell')))))]

      (reset! levels/level*
              (-> level
                  (assoc :layer/one layer-one')
                  (assoc-in [:sneks sname] snek'))))))
