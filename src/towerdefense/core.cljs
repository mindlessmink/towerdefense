(ns towerdefense.core
  (:require (towerdefense.creep :refer [Creep
                                        make-creep
                                        update-creeps])
            (towerdefense.field :refer [Target
                                        make-blockmap
                                        make-path-map
                                        make-target
                                        maybe-update-path-map])
            (towerdefense.input :refer [init-input
                                        process-inputs])
            (towerdefense.render :refer [render-game])
            (towerdefense.spawner :refer [init-spawners
                                          update-spawners])
            (towerdefense.tower :refer [Tower
                                        make-tower
                                        update-bullets
                                        update-towers])))

(defn update-state [state tick-time]
  (let [tick-seconds (/ tick-time 1000)]
    (-> state
        process-inputs
        maybe-update-path-map
        (update-spawners tick-seconds)
        (update-towers tick-seconds)
        (update-bullets tick-seconds)
        (update-creeps tick-time)
        (update :frames-rendered inc))))

(defn frame-callback [state old-timestamp]
  (fn [new-timestamp]
    (let [new-state (update-state state (- new-timestamp old-timestamp))]
      (render-game new-state)
      (.requestAnimationFrame js/window
                              (frame-callback new-state new-timestamp)))))

(def initial-state
  {:frames-rendered 0
   :towers []
   :creeps (hash-map)
   :bullets []
   :score 0
   :money 200
   :lives 20
   :targets (hash-set)
   :path-map nil})

(defn start-game [timestamp]
  (let [state (init-spawners initial-state)]
    (.requestAnimationFrame js/window (frame-callback state timestamp))))

(init-input)
(.requestAnimationFrame js/window start-game)
