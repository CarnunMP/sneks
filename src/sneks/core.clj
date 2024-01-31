(ns sneks.core
  (:require
    [clojure.set :as set]
    [play-cljc.gl.core :as c]
    [play-cljc.gl.entities-2d :as e]
    [play-cljc.macros-java :refer [gl]]
    [play-cljc.transforms :as t]
    [sneks.levels.levels :as levels]
    [sneks.move :as move]
    [sneks.utils :as utils]
    [sneks.utils.cells :as uc]
    [sneks.utils.sneks :as us]
    [sneks.utils.vectors :as uv]))

(defonce *state (atom {:mouse-x 0
                       :mouse-y 0
                       :mouse-button nil
                       :pressed-keys #{}
                       :x-velocity 0
                       :y-velocity 0
                       :player-x 0
                       :player-y 0
                       :can-jump? false
                       :direction :right
                       :player-images {}
                       :player-image-key :walk1}))

(comment
  (require '[clojure.pprint :refer [pprint]])
  (pprint @*state)
  )

(defn init [game]
  ;; allow transparency in images
  #_{:clj-kondo/ignore [:unresolved-symbol]}
  (gl game enable (gl game BLEND))
  #_{:clj-kondo/ignore [:unresolved-symbol]}
  (gl game blendFunc (gl game SRC_ALPHA) (gl game ONE_MINUS_SRC_ALPHA))

  ;; load images and put them in the state atom
  (doseq [[k path] {:walk1 "player_walk1.png"
                    :walk2 "player_walk2.png"
                    :walk3 "player_walk3.png"}]
    (utils/get-image path
      (fn [{:keys [data width height]}]
        (let [;; create an image entity (a map with info necessary to display it)
              entity (e/->image-entity game data width height)
              ;; compile the shaders so it is ready to render
              entity (c/compile game entity)
              ;; assoc the width and height to we can reference it later
              entity (assoc entity :width width :height height)]
          ;; add it to the state
          (swap! *state update :player-images assoc k entity))))))

(def screen-entity
  {:viewport {:x 0 :y 0 :width 0 :height 0}
   :clear {:color [(/ 173 255) (/ 216 255) (/ 230 255) 1] :depth 1}})

(defn tick [game]
  (let [{:keys [pressed-keys
                player-x
                player-y
                direction
                player-images
                player-image-key]
         :as state} @*state
        [game-width game-height] (utils/get-size game)]
    (when (and (pos? game-width) (pos? game-height))
      ;; render the blue background
      (c/render game (update screen-entity :viewport
                             assoc :width game-width :height game-height))
      ;; get the current player image to display
      (when-let [player (get player-images player-image-key)]
        (let [player-width (/ game-width 10)
              player-height (* player-width (/ (:height player) (:width player)))]
          ;; render the player
          (c/render game
            (-> player
                (t/project game-width game-height)
                (t/translate (cond-> player-x
                                     (= direction :left)
                                     (+ player-width))
                             player-y)
                (t/scale (cond-> player-width
                                 (= direction :left)
                                 (* -1))
                         player-height)))
          ;; change the state to move the player
          (swap! *state
            (fn [state]
              (->> (assoc state
                          :player-width player-width
                          :player-height player-height)
                   (move/move game)
                   (move/prevent-move game)
                   (move/animate game))))))))
  ;; return the game map
  game)


;; ---

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
        (uc/snek-cell? cell)
        (recur (inc x) y (assoc-in sneks [(first cell) (uc/snek-cell->sneg-num cell)] [x y]))

        ;; next cell
        :else
        (recur (inc x) y sneks)))))

;; TODO: move?
(defn load-level!
  "Loads the level into the level* atom."
  [level]
  (->> (assoc level :sneks (read-sneks (:layer/one level)))
       (reset! levels/level*)))

;; TODO: move?
(defn new-head
  "If the snamed snek can move in the given dir, returns its new head coord. Else nil."
  [sname dir]
  (let [adj-coord (uv/add (us/get-head sname) (uv/dir->x+y dir))
        adj-cell (uc/get-cell :layer/one adj-coord)]
    (when (uc/empty-cell? adj-cell)
      adj-coord)))

;; TODO: move?
(defn move-snek!
  "Moves the snamed snek in the given dir(ection) if it can move, returning the new value of level*. Else nil."
  [sname dir]
  {:pre [(#{:n :e :s :w} dir)]}

  ;; only move if _can_ move
  (when-let [new-head (new-head sname dir)]
    (let [level @levels/level*
          snek (us/get-snek sname)
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
                                     (uc/this-snek-cell? sname cell) \.
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

(comment
  (require '[clojure.pprint :refer [pprint]])

  (pprint @levels/level*)
  (pprint (load-level! levels/level-0))
  (pprint (move-snek! \a :e))
  )
