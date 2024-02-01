(ns sneks.levels.levels
  (:require
    [sneks.utils.sneks :as us]))

; layer one:
; - floor [nil]
; - goal(s) [\A, \B, etc.]

; layer two:
; - snake head [a0, b0, etc.]
; - snake tail [a1 a2 a3, etc.]
; - snack [s0, s1, etc.]
; - wall [\#]

; layer three:
; - ???

; TODO: maybe move to EDN?
(def level-0
  {:layer/zero [[\. \. \. \. \.]
                [\. \. \. \. \.]
                [\. \. \. \. \A]
                [\. \. \. \. \.]
                [\. \. \. \. \.]]
   :layer/one [[\. \. \. \. \.]
               ["a0" \. \. \. \.]
               ["a1" \. \. \. \.]
               ["a2" \. \. \. \.]
               [\. \. \. \. \.]]})

(def level* (atom nil))

(defn load-level!
  "Loads the level into the level* atom."
  [level]
  (->> (assoc level :sneks (us/read-sneks (:layer/one level)))
       (reset! level*)))
