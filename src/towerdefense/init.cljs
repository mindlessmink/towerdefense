(ns towerdefense.init
  (:require (towerdefense.spawner :refer [init-spawners])))

(def ^:private initial-state
  {:frames-rendered 0
   :walls (sorted-set)
   :towers (hash-map)
   :creeps (hash-map)
   :projectiles []
   :score 0
   :money 200
   :lives 20
   :path-map nil})

(defn- spawner-tiles [spawner]
  (let [start-tiles (:start-area spawner)
        target-tiles (get-in spawner [:target :tiles])]
    (reduce conj start-tiles target-tiles)))

(defn- add-walls [state]
  (let [spawner-tiles (map spawner-tiles (:spawners state))
        open-walls (reduce (fn [coll tiles]
                             (reduce conj coll tiles))
                           (sorted-set)
                           spawner-tiles)
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

(defn make-initial-state [game-over-fn]
  (-> (init-spawners initial-state)
      add-walls
      (assoc :game-over-fn game-over-fn)))
