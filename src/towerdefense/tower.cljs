(ns towerdefense.tower
  (:require (cljs.math :refer [sqrt])))

(def ^:private tower-defs
  {:pellet {:cost 5
            :fire-rate 1
            :damage 5
            :radius 5}
   :squirt {:cost 15
            :fire-rate 0.5
            :damage 10
            :radius 8}
   :dart {:cost 20
          :fire-rate 1.5
          :damage 20
          :radius 10}})

(defrecord Tower [tower-type tower-def level x y id time-since-last-bullet])

(defrecord Bullet [damage target coords])

(defn make-tower [tower-type level x y]
  (if-let [tower-def (get tower-defs tower-type)]
    (Tower. tower-type
            tower-def
            level
            x
            y
            (gensym (str (name tower-type) "tower"))
            1000)))

(defn tower-stat [tower-type stat]
  (get-in tower-defs [tower-type stat]))

(defn tower-cost [tower-type]
  (tower-stat tower-type :cost))

(defn tower-damage [tower-type]
  (tower-stat tower-type :damage))

(defn tower-fire-rate [tower-type]
  (tower-stat tower-type :fire-rate))

(defn tower-radius [tower-type]
  (tower-stat tower-type :radius))

(defn- update-timers [towers tick-seconds]
  (mapv (fn [tower]
          (update tower :time-since-last-bullet + tick-seconds))
        towers))

(defn- should-fire? [tower]
  (>= (:time-since-last-bullet tower) (tower-fire-rate (:tower-type tower))))

(defn- distance [[x1 y1] [x2 y2]]
  (let [a (- x2 x1)
        b (- y2 y1)]
    (sqrt (+ (* a a) (* b b)))))

(defn- creeps-in-radius [tower creeps]
  (let [tower-coords [(inc (:x tower)) (inc (:y tower))]
        radius (tower-radius (:tower-type tower))]
    (filter (fn [creep]
              (let [creep-coords (get-in creep [1 :coords])]
                (<= (distance tower-coords creep-coords) radius)))
            creeps)))

(defn- bullet-from-tower [tower creeps]
  (let [targetable-creeps (creeps-in-radius tower creeps)]
    (if (empty? targetable-creeps)
      nil
      (let [target (rand-nth targetable-creeps)] ; for now
        (Bullet. (tower-damage (:tower-type tower))
                 (first target)
                 [(inc (:x tower)) (inc (:y tower))])))))

(defn update-towers [state tick-seconds]
  (let [towers (:towers state)
        updated-timers (update-timers towers tick-seconds)
        creeps (:creeps state)]
    (if (empty? creeps)
      (assoc state :towers updated-timers)
      (let [firing-towers (filter should-fire? updated-timers)
            fired-towers (mapv (fn [tower]
                                 (if (should-fire? tower)
                                   (assoc tower :time-since-last-bullet 0)
                                   tower))
                               updated-timers)
            bullets (filterv (complement nil?)
                             (map #(bullet-from-tower % creeps) firing-towers))]
        (-> state
            (assoc :towers fired-towers)
            (update :bullets (fn [old-bullets]
                               (apply conj old-bullets bullets))))))))

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
      state
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
