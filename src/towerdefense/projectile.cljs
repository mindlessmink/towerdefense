(ns towerdefense.projectile
  (:require (towerdefense.field :refer [distance])))

(defrecord Projectile [damage target coords])

(def ^:private projectile-speed 15)

(defn movement-vector [[x1 y1] [x2 y2]]
  (let [[dx dy :as delta] [(- x2 x1) (- y2 y1)]
        dist (distance [x1 y1] [x2 y2])]
    [(/ dx dist) (/ dy dist)]))

(defn- move-projectile [projectile creeps tick-seconds]
  (let [target-creep (get creeps (:target projectile))
        [bx by :as projectile-coords] (:coords projectile)
        [cx cy :as creep-coords] (:coords target-creep)
        [dx dy :as delta] (movement-vector projectile-coords creep-coords)
        dist (* projectile-speed tick-seconds)]
    (assoc projectile :coords [(+ bx (* dist dx))
                           (+ by (* dist dy))])))

(defn- should-remove? [projectile creeps]
  (not (contains? creeps (:target projectile))))

(defn- move-projectiles [state tick-seconds]
  (let [projectiles (:projectiles state)]
    (if (empty? projectiles)
      projectiles
      (let [creeps (:creeps state)
            remaining-projectiles (remove #(should-remove? % creeps) projectiles)
            moved-projectiles (mapv #(move-projectile % creeps tick-seconds)
                                remaining-projectiles)]
        moved-projectiles))))

(defn- hits-target? [projectile creeps]
  (let [target (get creeps (:target projectile))]
    (< (distance (:coords projectile) (:coords target)) 1)))

(defn update-projectiles [state tick-seconds]
  (let [moved-projectiles (move-projectiles state tick-seconds)
        creeps (:creeps state)
        hitting-projectiles (filter #(hits-target? % creeps) moved-projectiles)
        remaining-projectiles (filterv #(not (hits-target? % creeps)) moved-projectiles)]
    (loop [projectiles hitting-projectiles
           updated-creeps creeps]
      (if (empty? projectiles)
        (-> state
            (assoc :projectiles remaining-projectiles)
            (assoc :creeps updated-creeps))
        (let [curr-projectile (first projectiles)
              target-id (:target curr-projectile)]
          (recur (rest projectiles)
                 (update creeps
                         target-id
                         (fn [creep]
                           (update creep :health - (:damage curr-projectile))))))))))
