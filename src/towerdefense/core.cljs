(ns towerdefense.core
  (:require (towerdefense.render :refer (render-game))))

(defn update-state [state tick-time]
  (update state :frames-rendered inc))

(defn frame-callback [state old-timestamp]
  (fn [new-timestamp]
    (let [new-state (update-state state (- new-timestamp old-timestamp))]
      (render-game new-state)
      (.requestAnimationFrame js/window
                              (frame-callback new-state new-timestamp)))))

(defn new-game []
  {:frames-rendered 0})

(defn start-game [timestamp]
  (.requestAnimationFrame js/window (frame-callback (new-game) timestamp)))

(.requestAnimationFrame js/window start-game)
