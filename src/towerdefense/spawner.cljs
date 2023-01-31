;; Creep spawner. This automatically sets up their targets etc

(ns towerdefense.spawner
  (:require (towerdefense.creep :refer [Creep
                                        make-creep])
            (towerdefense.field :refer [Target
                                        make-target])))

(defrecord Spawner [start-area target])

(defn init-spawners [state]
  (let [target (make-target [[39 13] [39 14] [39 15] [39 16]])]
    (-> state
        (assoc :spawner (Spawner. [[0 13] [0 14] [0 15] [0 16]]
                                  target))
        (update :targets conj target))))

(defn- random-pixel [tile]
  (let [[x y] tile]
    [(+ (* 16 x) (rand-int 16))
     (+ (* 16 y) (rand-int 16))]))

(defn spawn-creep [spawner state]
  (let [start-tile (rand-nth (:start-area spawner))
        creep (make-creep (rand-nth [:normal :fast :immune :group])
                          (inc (rand-int 50))
                          (random-pixel start-tile)
                          (:target spawner))]
    (update state :creeps conj creep)))
