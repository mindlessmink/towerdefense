(ns towerdefense.projectile
  (:require (towerdefense.creep :refer [creeps-in-radius])
            (towerdefense.field :refer [distance])))

(defprotocol Projectile
  (hits? [projectile creeps])
  (hit [projectile creeps]))

(defrecord Bullet [damage target coords])

(defrecord Dart [damage radius target coords])

(defrecord FrostBullet [damage duration target coords])

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

(extend-type Bullet
  Projectile
  (hits? [bullet creeps]
    (let [target (get creeps (:target bullet))]
      (< (distance (:coords bullet) (:coords target)) 1)))
  (hit [bullet creeps]
    (update creeps (:target bullet) #(update % :health - (:damage bullet)))))

(extend-type Dart
  Projectile
  (hits? [dart creeps]
    (let [target (get creeps (:target dart))]
      (< (distance (:coords dart) (:coords target)) 1)))
  (hit [dart creeps]
    (let [targets (creeps-in-radius (:coords dart) (:radius dart) creeps)]
      (reduce (fn [creeps [id _]]
                (update creeps id #(update % :health - (:damage dart))))
              creeps
              targets))))

(extend-type FrostBullet
  Projectile
  (hits? [bullet creeps]
    (let [target (get creeps (:target bullet))]
      (< (distance (:coords bullet) (:coords target)) 1)))
  (hit [bullet creeps]
    (let [id (:target bullet)
          creep (get creeps id)
          damaged-creep (update creep :health - (:damage bullet))
          frosted-creep (if (= :immune (:creep-type creep))
                          damaged-creep
                          (assoc damaged-creep :frosted (:duration bullet)))]
      (assoc creeps id frosted-creep))))

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
