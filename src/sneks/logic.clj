(ns sneks.logic
  (:require
    [clojure.set :as set]
    [sneks.levels.levels :as l]
    [sneks.utils.logic :as ul]
    [sneks.utils.vectors :as uv]))

(defn new-head
  "If the snamed snek can move in the given dir, returns its new head coord. Else nil."
  [sname dir]
  (let [level @l/level*
        adj-coord (uv/add (ul/get-head level sname) (uv/dir->x+y dir))
        adj-cell (ul/get-layer-one-cell level adj-coord)]
    (when (ul/empty-cell? adj-cell)
      adj-coord)))

(defn move-snek!
  "Moves the snamed snek in the given dir(ection) if it can move, returning the new value of level*. Else nil."
  [sname dir]
  {:pre [(#{:n :e :s :w} dir)]}

  ;; only move if _can_ move
  (when-let [new-head (new-head sname dir)]
    (let [level @l/level*
          snek (ul/get-snek level sname)
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
                                     (ul/this-snek-cell? sname cell) \.
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

      (reset! l/level*
              (-> level
                  (assoc :layer/one layer-one')
                  (assoc-in [:sneks sname] snek'))))))
