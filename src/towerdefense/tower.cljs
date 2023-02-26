(ns towerdefense.tower
  (:require (cljs.math :refer [sqrt])
            (towerdefense.projectile :refer [Projectile])
            (towerdefense.field :refer [distance])))

(defn- def-tower [stats & stat-defs]
  (mapv (partial zipmap stats) stat-defs))

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

(defrecord Tower [tower-type tower-def level x y time-since-last-shot])

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

(defn upgradeable? [tower]
  (< (:level tower) (count (:tower-def tower))))

(defn upgrade-cost [tower]
  (get-in (:tower-def tower) [(:level tower) :cost]))

(defn upgrade-tower [tower]
  (update tower :level inc))

(defn- update-timers [towers tick-seconds]
  (reduce-kv (fn [m id tower]
               (assoc m id (update tower :time-since-last-shot + tick-seconds)))
             (hash-map)
             towers))

(defn- should-fire? [tower]
  (>= (:time-since-last-shot tower) (tower-fire-rate tower)))

(defn- creeps-in-radius [tower creeps]
  (let [tower-coords [(inc (:x tower)) (inc (:y tower))]
        radius (tower-radius tower)]
    (filter (fn [creep]
              (let [creep-coords (get-in creep [1 :coords])]
                (<= (distance tower-coords creep-coords) radius)))
            creeps)))

(defn- projectile-from-tower [tower creeps]
  (let [targetable-creeps (creeps-in-radius tower creeps)]
    (if (empty? targetable-creeps)
      nil
      (let [target (rand-nth targetable-creeps)] ; for now
        (Projectile. (tower-damage tower)
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
                                  [id (assoc tower :time-since-last-shot 0)]
                                  [id tower]))
                              updated-timers)
            projectiles (filterv (complement nil?)
                                 (map #(projectile-from-tower % creeps)
                                      (map second firing-towers)))]
        (-> state
            (assoc :towers (reduce conj (hash-map) fired-towers))
            (update :projectiles (fn [old-projectiles]
                                   (apply conj old-projectiles projectiles))))))))
