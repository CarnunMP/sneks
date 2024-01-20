(ns user
  (:require [sneks.start :as start]
            [sneks.core :as c]
            [clojure.spec.test.alpha :as st]
            [clojure.spec.alpha :as s]
            [play-cljc.gl.core :as pc])
  (:import [org.lwjgl.glfw GLFW]
           [sneks.start Window]))

(defn start []
  (st/instrument)
  (let [window (start/->window)
        game (pc/->game (:handle window))]
    (start/start game window)))
