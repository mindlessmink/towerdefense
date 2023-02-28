(ns towerdefense.creep
  (:require (cljs.math :refer [ceil
                               floor
                               pow
                               round])
            (towerdefense.field :refer [Target
                                        blocked?
                                        distance
                                        find-path
                                        find-path-on-map
                                        in-target?
                                        make-path-map
                                        pixel->tile])))

(defrecord Creep [creep-type health max-health wave points money boss? coords target])

(defn- creep-health [creep-type wave boss?]
  (let [base (* 20 (pow 1.10 (dec wave)))]
    (if boss?
      (* 10 base)
      base)))

(defn- creep-point-value [creep-type wave boss?]
  (if boss?
    (* 10 wave)
    wave))

(defn- creep-money-value [creep-type wave boss?]
  (let [base-value (ceil (/ (inc wave) 8))]
    (if boss?
      (* base-value 20)
      base-value)))

(defn make-creep [creep-type wave boss? coords target]
  (let [health (creep-health creep-type wave boss?)]
    (Creep. creep-type
            health
            health
            wave
            (creep-point-value creep-type wave boss?)
            (creep-money-value creep-type wave boss?)
            boss?
            coords
            target)))

(defn creep-tile
  "Returns the tile the creep is on"
  [creep]
  (mapv floor (:coords creep)))

(def ^:private creep-colors {:normal "silver"
                             :group "blue"
                             :fast "red"
                             :immune "fuchsia"
                             :flying "yellow"
                             :spawn "lime"
                             :dark "black"})

(defn creep-color [creep]
  (get creep-colors (:creep-type creep) "white"))

(def ^:private creep-speeds {:normal 4
                             :group 4
                             :fast 6
                             :immune 4
                             :flying 4
                             :spawn 3
                             :dark 2})

;; Tiles per second
(defn- creep-speed [creep]
  (let [base-speed (get creep-speeds (:creep-type creep) 4)
        speed (if (:boss? creep)
                (* base-speed 0.8)
                base-speed)]
    (if (:frosted creep)
      (* speed 0.5)
      speed)))

(defn creep-armor [creep]
  (if-not (= :dark (:creep-type creep))
    0
    (* 5 (inc (floor (/ (:wave creep) 10))))))

(defn damage-creep [creep damage]
  (update creep :health - (max 1 (- damage (creep-armor creep)))))

(defn- get-delta [state path-map creep]
  (let [[x y :as coords] (creep-tile creep)
        path (find-path-on-map path-map coords)
        [nx ny] (first path)]
    [(- nx x) (- ny y)]))

(defn- make-targets [[x y] dist [dx dy]]
  [[(+ x (* dx dist)) (+ y (* dy dist))]
   [x (+ y (* dy dist))]
   [(+ x (* dx dist)) y]
   [x y]])

(defn- find-closest-tile [start-tile tiles]
  (if (empty? tiles)
    nil
    (loop [closest-tile (first tiles)
           closest-distance (distance start-tile closest-tile)
           tiles (rest tiles)]
      (if (empty? tiles)
        closest-tile
        (let [curr-tile (first tiles)
              curr-distance (distance start-tile curr-tile)]
          (recur (if (< curr-distance closest-distance) curr-tile closest-tile)
                 (min curr-distance closest-distance)
                 (rest tiles)))))))

(defn- get-delta-flying [creep [tx ty]]
  (let [[x y] (creep-tile creep)]
    [(cond
       (= x tx) 0
       (< x tx) 1
       :else -1)
     (cond
       (= y ty) 0
       (< y ty) 1
       :else -1)]))

(defn- move-creep-flying [state creep tick-seconds]
  (let [target (:target creep)
        [x y] (:coords creep)
        closest-tile (find-closest-tile (:coords creep) (:tiles target))
        [dx dy] (get-delta-flying creep closest-tile)
        dist (* tick-seconds (creep-speed creep))]
    (assoc creep :coords [(+ x (* dx dist)) (+ y (* dy dist))])))

(defn- move-creep [state path-map creep tick-seconds]
  (if (= :flying (:creep-type creep))
    (move-creep-flying state creep tick-seconds)
    (let [coords (:coords creep)
          dist (* (creep-speed creep) tick-seconds) ; how many pixels this moves
          delta (get-delta state path-map creep)
          possible-targets (remove #(blocked? state (mapv floor %)) (make-targets coords dist delta))]
      (assoc creep :coords (first possible-targets)))))

(defn- dead? [creep]
  (<= (:health creep) 0))

(defn- finished? [creep]
  (and (in-target? (:target creep) (creep-tile creep))
       (not (dead? creep))))

(defn- move-creeps [state tick-seconds]
  (let [creeps (:creeps state)
        path-maps (:path-maps state)]
    (assoc state
           :creeps 
           (apply conj
                   creeps
                   (map (fn [creep]
                          [(first creep)
                           (move-creep state
                                       (get path-maps (get-in creep [1 :target]))
                                       (second creep)
                                       tick-seconds)])
                        creeps)))))

(defn- splitting-spawn? [creep]
  (and (= :spawn (:creep-type creep))
       (< (get creep :spawn-level 0) 2) ;; spawns split twice for now.
       (dead? creep)))

(defn- random-coords-in-tile [[x y]]
  (let [[floored-x floored-y] [(floor x) (floor y)]]
    [(+ floored-x (rand)) (+ floored-y (rand))]))

(defn- make-spawn-baby [creep]
  (let [health (floor (/ (:max-health creep) 2))
        level (inc (get creep :spawn-level 0))]
    (assoc creep
           :health health
           :max-health health
           :points 0 ; could maybe give a few points for the babies too
           :money 0
           :spawn-level level
           :coords (random-coords-in-tile (:coords creep)))))

(defn- split-dead-spawns [state]
  (let [creeps (:creeps state)
        splitting-spawns (filter #(splitting-spawn? (second %)) creeps)]
    (loop [new-creeps []
           spawns splitting-spawns]
      (if (empty? spawns)
        (assoc state
               :creeps
               (apply conj creeps new-creeps))
        (let [[_ curr-spawn] (first spawns)]
          (recur (apply conj
                        new-creeps
                        [[(gensym "spawn-baby") (make-spawn-baby curr-spawn)]
                         [(gensym "spawn-baby") (make-spawn-baby curr-spawn)]])
                 (next spawns)))))))

(defn- update-frost-timers [state tick-seconds]
  (let [creeps (:creeps state)
        updated-creeps (reduce (fn [coll [id creep]]
                                 (let [updated-creep (if-let [frost-timer (:frosted creep)]
                                                       (let [new-timer (- frost-timer tick-seconds)]
                                                         (if (pos? new-timer)
                                                           (assoc creep :frosted new-timer)
                                                           (dissoc creep :frosted)))
                                                       creep)]
                                   (assoc coll id updated-creep)))
                               creeps
                               creeps)]
    (assoc state :creeps updated-creeps)))
                                      
;; Remove creeps that are at their target
(defn- remove-creeps [state]
  (let [creeps (:creeps state)
        dead-creeps (filter #(dead? (second %)) creeps)
        dead-creeps-points (apply + (map #(get-in % [1 :points])
                                         dead-creeps))
        dead-creeps-money (apply + (map #(get-in % [1 :money])
                                        dead-creeps))
        finished-creeps (filter #(finished? (second %)) creeps)]
    (-> state
        (update :score + dead-creeps-points)
        (update :money + dead-creeps-money)
        (update :lives - (count finished-creeps))
        (update :creeps (fn [creeps]
                          (apply dissoc creeps (map first dead-creeps))))
        (update :creeps (fn [creeps]
                          (apply dissoc creeps (map first finished-creeps)))))))

(defn update-creeps [state tick-seconds]
  (-> state
      (move-creeps tick-seconds)
      split-dead-spawns
      (update-frost-timers tick-seconds)
      remove-creeps))

(defn creeps-in-radius [coords radius creeps]
  (filter (fn [creep]
            (let [creep-coords (get-in creep [1 :coords])]
              (<= (distance coords creep-coords) radius)))
          creeps))
