(ns towerdefense.core
  (:require (towerdefense.creep :refer [Creep
                                        make-creep
                                        update-creeps])
            (towerdefense.field :refer [Target
                                        make-path-map
                                        make-target])
            (towerdefense.input :refer [init-input
                                        process-inputs])
            (towerdefense.render :refer [render-game])
            (towerdefense.spawner :refer [init-spawners
                                          update-spawners])
            (towerdefense.tower :refer [Tower
                                        make-tower])))

(defn maybe-update-path-map [state]
  (let [path-map (:path-map state)
        old-towers (:old-towers state)]
    (if (or (nil? path-map) (not= old-towers (:towers state)))
      (assoc state
             :path-map (make-path-map state (:tiles (first (:targets state))))
             :old-towers (:towers state))
      state)))

(defn update-state [state tick-time]
  (-> state
      process-inputs
      maybe-update-path-map
      (update-spawners (/ tick-time 1000))
      (update-creeps tick-time)
      (update :frames-rendered inc)))

(defn frame-callback [state old-timestamp]
  (fn [new-timestamp]
    (let [new-state (update-state state (- new-timestamp old-timestamp))]
      (render-game new-state)
      (.requestAnimationFrame js/window
                              (frame-callback new-state new-timestamp)))))

(def initial-state
  {:frames-rendered 0
   :towers []
   :creeps []
   :money 200
   :lives 20
   :targets (hash-set)
   :path-map nil})

(defn start-game [timestamp]
  (let [state (init-spawners initial-state)]
    (.requestAnimationFrame js/window (frame-callback state timestamp))))

(init-input)
(.requestAnimationFrame js/window start-game)
