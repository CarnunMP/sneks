(ns sneks.utils.sneks
  (:require [sneks.levels.levels :as levels]))

(def sname-set #{\a \b \c})

(defn get-snek [sname]
  (get-in @levels/level* [:sneks sname]))

(defn get-head [sname]
  (get (get-snek sname) 0))
