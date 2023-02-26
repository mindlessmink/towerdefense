(ns towerdefense.core
  (:require (towerdefense.projectile :refer [update-projectiles])
            (towerdefense.creep :refer [Creep
                                        make-creep
                                        update-creeps])
            (towerdefense.field :refer [Target
                                        make-blockmap
                                        make-path-map
                                        make-target
                                        maybe-update-path-maps])
            (towerdefense.init :refer [make-initial-state])
            (towerdefense.input :refer [init-input
                                        process-inputs])
            (towerdefense.render :refer [render-game])
            (towerdefense.spawner :refer [update-spawners])
            (towerdefense.tower :refer [Tower
                                        make-tower
                                        update-towers])))

(defn update-state [state tick-time]
  (let [tick-seconds (/ tick-time 1000)]
    (-> state
        process-inputs
        maybe-update-path-maps
        (update-spawners tick-seconds)
        (update-towers tick-seconds)
        (update-projectiles tick-seconds)
        (update-creeps tick-seconds)
        (update :frames-rendered inc))))

(defn frame-callback [state old-timestamp]
  (fn [new-timestamp]
    (let [new-state (update-state state (- new-timestamp old-timestamp))]
      (render-game new-state)
      (.requestAnimationFrame js/window
                              (frame-callback new-state new-timestamp)))))

(defn start-game [timestamp]
    (.requestAnimationFrame js/window (frame-callback (make-initial-state)
                                                      timestamp)))

(init-input)
(.requestAnimationFrame js/window start-game)
