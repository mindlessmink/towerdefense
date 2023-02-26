;;; Bullet in this case means anything shot by towers. Maybe "projectile"
;;; would be a better name but bullet is easier.

(ns towerdefense.bullet
  (:require (towerdefense.field :refer [distance])))

(defrecord Bullet [damage target coords])

(def ^:private bullet-speed 15)

(defn movement-vector [[x1 y1] [x2 y2]]
  (let [[dx dy :as delta] [(- x2 x1) (- y2 y1)]
        dist (distance [x1 y1] [x2 y2])]
    [(/ dx dist) (/ dy dist)]))

(defn- move-bullet [bullet creeps tick-seconds]
  (let [target-creep (get creeps (:target bullet))
        [bx by :as bullet-coords] (:coords bullet)
        [cx cy :as creep-coords] (:coords target-creep)
        [dx dy :as delta] (movement-vector bullet-coords creep-coords)
        dist (* bullet-speed tick-seconds)]
    (assoc bullet :coords [(+ bx (* dist dx))
                           (+ by (* dist dy))])))

(defn- should-remove? [bullet creeps]
  (not (contains? creeps (:target bullet))))

(defn- move-bullets [state tick-seconds]
  (let [bullets (:bullets state)]
    (if (empty? bullets)
      bullets
      (let [creeps (:creeps state)
            remaining-bullets (remove #(should-remove? % creeps) bullets)
            moved-bullets (mapv #(move-bullet % creeps tick-seconds)
                                remaining-bullets)]
        moved-bullets))))

(defn- hits-target? [bullet creeps]
  (let [target (get creeps (:target bullet))]
    (< (distance (:coords bullet) (:coords target)) 1)))

(defn update-bullets [state tick-seconds]
  (let [moved-bullets (move-bullets state tick-seconds)
        creeps (:creeps state)
        hitting-bullets (filter #(hits-target? % creeps) moved-bullets)
        remaining-bullets (filterv #(not (hits-target? % creeps)) moved-bullets)]
    (loop [bullets hitting-bullets
           updated-creeps creeps]
      (if (empty? bullets)
        (-> state
            (assoc :bullets remaining-bullets)
            (assoc :creeps updated-creeps))
        (let [curr-bullet (first bullets)
              target-id (:target curr-bullet)]
          (recur (rest bullets)
                 (update creeps
                         target-id
                         (fn [creep]
                           (update creep :health - (:damage curr-bullet))))))))))
