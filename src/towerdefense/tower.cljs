(ns towerdefense.tower
  (:require (cljs.math :refer [sqrt])))

(defn- def-tower [stats & stat-defs]
  (loop [stat-maps []
         remaining-defs stat-defs]
    (if (empty? remaining-defs)
      stat-maps
      (recur (conj stat-maps (zipmap stats (first remaining-defs)))
             (rest remaining-defs)))))

(def ^:private tower-defs
  {:pellet (def-tower [:cost :fire-rate :damage :radius]
             [5 1 5 5]
             [10 1 10 5]
             [20 1 20 5]
             [40 1 40 5]
             [80 1 80 5]
             [160 2 400 15])
   :squirt (def-tower [:cost :fire-rate :damage :radius]
             [15 0.5 5 8]
             [15 0.5 10 8]
             [35 0.5 18 8]
             [50 0.5 32 8]
             [120 0.5 60 8]
             [400 0.5 250 8])
   :dart (def-tower [:cost :fire-rate :damage :radius]
           [20 1.5 10 10]
           [20 1.5 20 11]
           [50 1.5 40 12]
           [100 1.5 80 13]
           [150 1.5 160 14]
           [500 1.5 320 15])})

(defrecord Tower [tower-type tower-def level x y time-since-last-bullet])

(defrecord Bullet [damage target coords])

(defn make-tower [tower-type level x y]
  (if-let [tower-def (get tower-defs tower-type)]
    (Tower. tower-type
            tower-def
            level
            x
            y
            1000)))

(defn tower-stat [tower stat]
  (get-in (:tower-def tower) [(dec (:level tower)) stat]))

(defn tower-build-cost [tower-type]
  (get-in tower-defs [tower-type 0 :cost]))

(defn tower-cost [tower]
  (tower-stat tower :cost))

(defn tower-damage [tower]
  (tower-stat tower :damage))

(defn tower-fire-rate [tower]
  (tower-stat tower :fire-rate))

(defn tower-radius [tower]
  (tower-stat tower :radius))

(defn- update-timers [towers tick-seconds]
  (reduce-kv (fn [m id tower]
               (assoc m id (update tower :time-since-last-bullet + tick-seconds)))
             (hash-map)
             towers))

(defn- should-fire? [tower]
  (>= (:time-since-last-bullet tower) (tower-fire-rate tower)))

(defn- distance [[x1 y1] [x2 y2]]
  (let [a (- x2 x1)
        b (- y2 y1)]
    (sqrt (+ (* a a) (* b b)))))

(defn- creeps-in-radius [tower creeps]
  (let [tower-coords [(inc (:x tower)) (inc (:y tower))]
        radius (tower-radius tower)]
    (filter (fn [creep]
              (let [creep-coords (get-in creep [1 :coords])]
                (<= (distance tower-coords creep-coords) radius)))
            creeps)))

(defn- bullet-from-tower [tower creeps]
  (let [targetable-creeps (creeps-in-radius tower creeps)]
    (if (empty? targetable-creeps)
      nil
      (let [target (rand-nth targetable-creeps)] ; for now
        (Bullet. (tower-damage tower)
                 (first target)
                 [(inc (:x tower)) (inc (:y tower))])))))

(defn update-towers [state tick-seconds]
  (let [towers (:towers state)
        updated-timers (update-timers towers tick-seconds)
        creeps (:creeps state)]
    (if (empty? creeps)
      (assoc state :towers updated-timers)
      (let [firing-towers (filter #(should-fire? (second %)) updated-timers)
            fired-towers (map (fn [[id tower]]
                                (if (should-fire? tower)
                                  [id (assoc tower :time-since-last-bullet 0)]
                                  [id tower]))
                              updated-timers)
            bullets (filterv (complement nil?)
                             (map #(bullet-from-tower % creeps)
                                  (map second firing-towers)))]
        (-> state
            (assoc :towers (reduce conj (hash-map) fired-towers))
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
