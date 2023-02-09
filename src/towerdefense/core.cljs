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
   :walls (sorted-set)
   :towers (hash-map)
   :creeps (hash-map)
   :bullets []
   :score 0
   :money 200
   :lives 20
   :path-map nil})

(defn- add-walls [state]
  (let [spawner (:spawner state)
        start-tiles (:start-area spawner)
        target-tiles (get-in spawner [:target :tiles])
        open-walls (apply conj
                          (apply conj (sorted-set) start-tiles)
                          target-tiles)
        tiles (for [x (range 40)
                    y (range 30)
                    :when (or (= x 0)
                              (= x 39)
                              (= y 0)
                              (= y 29))]
                [x y])]
    (assoc state
           :walls
           (reduce conj
                   (sorted-set)
                   (filterv (fn [tile]
                              (not (contains? open-walls tile)))
                            tiles)))))

(defn start-game [timestamp]
  (let [state (-> (init-spawners initial-state)
                  add-walls)]
    (.requestAnimationFrame js/window (frame-callback state timestamp))))

(init-input)
(.requestAnimationFrame js/window start-game)
