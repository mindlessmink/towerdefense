;; Creep spawner. This automatically sets up their targets etc

(ns towerdefense.spawner
  (:require (towerdefense.creep :refer [Creep
                                        make-creep])
            (towerdefense.field :refer [Target
                                        make-target])))

(defrecord Wave [wave-num creep-type creeps-left boss? time-since-last-spawn])

(defrecord Spawner [start-area target curr-wave-num waves time-since-last-wave])

(def ^:private right-target (make-target [[39 13] [39 14] [39 15] [39 16]]))
(def ^:private bottom-target (make-target [[18 29] [19 29] [20 29] [21 29]]))

(def ^:private left-spawner (Spawner. [[0 13] [0 14] [0 15] [0 16]]
                                      right-target
                                      0
                                      []
                                      nil))

(def ^:private top-spawner (Spawner. [[18 0] [19 0] [20 0] [21 0]]
                                     bottom-target
                                     0
                                     []
                                     nil))

(defn init-spawners [state]
  (assoc state :spawners [left-spawner top-spawner]))

(defn- random-pixel [tile]
  (let [[x y] tile]
    [(+ x (rand))
     (+ y (rand))]))

(def ^:private wave-types [:immune :normal :fast :group])

(defn- get-wave-by-num [wave-num]
  (let [creep-type (get wave-types (mod wave-num (count wave-types)))
        boss? (zero? (mod wave-num 7))
        num-creeps (if-not boss?
                     10
                     (if (= creep-type :group)
                       3
                       1))]
    (Wave. wave-num creep-type num-creeps boss? 1000000)))

(defn describe-wave [wave-num]
  (let [wave (get-wave-by-num wave-num)]
    (str (name (:creep-type wave))
         (if (:boss? wave) " (BOSS)" ""))))

(defn start-next-wave [spawner]
  (let [prev-wave-num (:curr-wave-num spawner)
        next-wave-num (inc prev-wave-num)
        next-wave (get-wave-by-num next-wave-num)]
    (conj spawner
          [:curr-wave-num next-wave-num]
          [:waves (conj (:waves spawner) next-wave)]
          [:time-since-last-wave 0])))

;; seconds between waves
(def wave-time 30)

;; seconds between creeps
(def ^:private creep-time 1)
(def ^:private group-creep-time 0.1)

(defn- maybe-spawn-new-wave [spawner tick-seconds]
  (let [timer (:time-since-last-wave spawner)]
    (if (nil? timer)
      spawner ; game hasn't started yet
      (let [new-timer (+ timer tick-seconds)]
        (if (< new-timer wave-time)
          (assoc spawner :time-since-last-wave new-timer)
          (start-next-wave spawner))))))

(defn- creep-from-wave [spawner wave]
  (let [start-tile (rand-nth (:start-area spawner))
        start-coords (random-pixel start-tile)]
  (make-creep (:creep-type wave)
              (:wave-num wave)
              (:boss? wave)
              start-coords
              (:target spawner))))

(defn- should-spawn-creep? [wave]
  (>= (:time-since-last-spawn wave)
      (if (= :group (:creep-type wave))
        group-creep-time
        creep-time)))

(defn- update-waves [waves]
  (let [spawned-waves (map (fn [wave]
                             (if (should-spawn-creep? wave)
                               (conj wave
                                     [:time-since-last-spawn 0]
                                     [:creeps-left (dec (:creeps-left wave))])
                               wave))
                           waves)
        filtered-waves (filterv #(> (:creeps-left %) 0) spawned-waves)]
    filtered-waves))

(defn- make-creep-entry [creep]
  [(gensym "creep") creep])

(defn- maybe-spawn-creeps [state tick-seconds]
  (let [spawners (:spawners state)]
    (loop [remaining-spawners spawners
           updated-spawners []
           new-creeps []]
      (if (empty? remaining-spawners)
        (assoc state
               :creeps (reduce conj (:creeps state) new-creeps)
               :spawners updated-spawners)
        (let [spawner (first remaining-spawners)
              waves (:waves spawner)
              new-waves (map #(update % :time-since-last-spawn + tick-seconds)
                             waves)
              spawning-waves (filter should-spawn-creep? new-waves)
              spawned-creeps (map #(creep-from-wave spawner %) spawning-waves)
              updated-waves (update-waves new-waves)
              new-spawner (assoc spawner :waves updated-waves)]
          (recur (rest remaining-spawners)
                 (conj updated-spawners new-spawner)
                 (reduce conj
                         new-creeps
                         (map make-creep-entry spawned-creeps))))))))

(defn update-spawners [state tick-seconds]
  (let [spawners (mapv #(maybe-spawn-new-wave % tick-seconds)
                       (:spawners state))
        new-state (assoc state :spawners spawners)]
    (maybe-spawn-creeps new-state tick-seconds)))
