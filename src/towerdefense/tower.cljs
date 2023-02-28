(ns towerdefense.tower
  (:require (cljs.math :refer [sqrt])
            (towerdefense.creep :refer [creeps-in-radius])
            (towerdefense.field :refer [distance])
            (towerdefense.projectile :refer [Bullet
                                             Dart
                                             FrostBullet
                                             Projectile])))

(defn- def-tower [stats & stat-defs]
  (mapv (partial zipmap stats) stat-defs))

(def ^:private tower-defs
  {:pellet (def-tower [:cost :fire-rate :damage :radius]
             [5 1 10 5]
             [10 1 20 5]
             [20 1 40 5]
             [40 1 80 5]
             [80 1 160 5]
             [160 2 400 15])
   :squirt (def-tower [:cost :fire-rate :damage :radius]
             [15 0.5 8 8]
             [15 0.5 15 8]
             [35 0.5 30 8]
             [50 0.5 50 8]
             [100 0.5 80 8]
             [300 0.5 300 8])
   :dart (def-tower [:cost :fire-rate :damage :dart-radius :radius]
           [20 1.5 10 1.5 10]
           [20 1.5 20 1.5 11]
           [50 1.5 40 1.5 12]
           [100 1.5 80 1.5 13]
           [150 1.5 160 1.5 14]
           [500 1.5 320 2.0 15])
   :swarm (def-tower [:cost :fire-rate :damage :radius]
            [50 0.8 15 8]
            [50 0.8 30 8]
            [75 0.8 60 8]
            [100 0.8 100 8]
            [150 0.8 150 8]
            [400 0.6 400 10])
   :frost (def-tower [:cost :fire-rate :damage :frost-duration :radius]
            [50 1 5 2.0 8]
            [50 1 10 3.5 8]
            [50 1 15 5.0 8]
            [50 1 20 6.5 8]
            [50 1 25 8.0 8]
            [150 1 30 12.0 10])})

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

(defn tower-dart-radius [tower]
  (tower-stat tower :dart-radius))

(defn tower-frost-duration [tower]
  (tower-stat tower :frost-duration))

(defn tower-fire-rate [tower]
  (tower-stat tower :fire-rate))

(defn tower-radius [tower]
  (tower-stat tower :radius))

(defn tower-radius-by-type [tower-type]
  (get-in tower-defs [tower-type 0 :radius]))

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

(defn- available-targets [tower creeps]
  (let [targets (creeps-in-radius [(inc (:x tower)) (inc (:y tower))] (tower-radius tower) creeps)
        ;; Some towers don't hit all types of creeps.
        ;; (Note that frost towers still hit immune creeps for minimal damage,
        ;; it's just the frost effect that doesn't work.)
        pred (case (:tower-type tower)
               :swarm #(= :flying (:creep-type (second %)))
               :dart #(not= :flying (:creep-type (second %)))
               any?)]
    (filter pred targets)))

(defn- projectile-from-tower [tower creeps]
  (let [targetable-creeps (available-targets tower creeps)]
    (if (empty? targetable-creeps)
      nil
      (let [target (rand-nth targetable-creeps)] ; for now
        (case (:tower-type tower)
          :dart (Dart. (tower-damage tower)
                       (tower-dart-radius tower)
                       (first target)
                       [(inc (:x tower)) (inc (:y tower))])
          :frost (FrostBullet. (tower-damage tower)
                               (tower-frost-duration tower)
                               (first target)
                               [(inc (:x tower)) (inc (:y tower))])
          (Bullet. (tower-damage tower)
                   (first target)
                   [(inc (:x tower)) (inc (:y tower))]))))))

(defn update-towers [state tick-seconds]
  (let [towers (:towers state)
        updated-timers (update-timers towers tick-seconds)
        creeps (:creeps state)]
    (if (empty? creeps)
      (assoc state :towers updated-timers)
      (let [firing-towers (filter #(should-fire? (second %)) updated-timers)
            ;; Swarm towers fire multiple shots. As a quick hack we can just
            ;; add them to the firing towers list several times.
            firing-towers (reduce (fn [coll [id tower]]
                                    (if (= :swarm (:tower-type tower))
                                      (conj coll [id tower] [id tower])
                                      coll))
                                  firing-towers
                                  firing-towers)
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
