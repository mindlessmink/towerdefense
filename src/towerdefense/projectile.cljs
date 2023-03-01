(ns towerdefense.projectile
  (:require (towerdefense.creep :refer [creeps-in-radius
                                        damage-creep])
            (towerdefense.field :refer [distance
                                        unit-vector])))

(defprotocol Projectile
  (hits? [projectile creeps])
  (hit [projectile creeps]))

(defrecord Bullet [damage target coords])

(defrecord Dart [damage radius target coords])

(defrecord FrostBullet [damage duration target coords])

(def ^:private projectile-speed 15)

(defn- move-projectile [projectile creeps tick-seconds]
  (let [target-creep (get creeps (:target projectile))
        [bx by :as projectile-coords] (:coords projectile)
        [cx cy :as creep-coords] (:coords target-creep)
        [dx dy :as delta] (unit-vector projectile-coords creep-coords)
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

(extend-type Bullet
  Projectile
  (hits? [bullet creeps]
    (let [target (get creeps (:target bullet))]
      (< (distance (:coords bullet) (:coords target)) 1)))
  (hit [bullet creeps]
    (update creeps (:target bullet) #(damage-creep % (:damage bullet)))))

(extend-type Dart
  Projectile
  (hits? [dart creeps]
    (let [target (get creeps (:target dart))]
      (< (distance (:coords dart) (:coords target)) 1)))
  (hit [dart creeps]
    (let [targets (creeps-in-radius (:coords dart) (:radius dart) creeps)]
      (reduce (fn [creeps [id _]]
                (update creeps id #(damage-creep % (:damage dart))))
              creeps
              targets))))

(defn- freeze-creep [creep duration]
  (if (= :immune (:creep-type creep))
    creep
    (let [old-duration (get creep :frosted 0.0)]
      (assoc creep :frosted (max duration old-duration)))))

(extend-type FrostBullet
  Projectile
  (hits? [bullet creeps]
    (let [target (get creeps (:target bullet))]
      (< (distance (:coords bullet) (:coords target)) 1)))
  (hit [bullet creeps]
    (let [id (:target bullet)
          creep (-> (get creeps id)
                    (damage-creep (:damage bullet))
                    (freeze-creep (:duration bullet)))]
      (assoc creeps id creep))))

(defn update-projectiles [state tick-seconds]
  (let [moved-projectiles (move-projectiles state tick-seconds)
        creeps (:creeps state)
        hitting-projectiles (filter #(hits? % creeps) moved-projectiles)
        remaining-projectiles (filterv #(not (hits? % creeps)) moved-projectiles)]
    (loop [projectiles hitting-projectiles
           updated-creeps creeps]
      (if (empty? projectiles)
        (-> state
            (assoc :projectiles remaining-projectiles)
            (assoc :creeps updated-creeps))
        (let [curr-projectile (first projectiles)]
          (recur (rest projectiles)
                 (hit curr-projectile creeps)))))))
