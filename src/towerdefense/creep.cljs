(ns towerdefense.creep
  (:require (cljs.math :refer [ceil
                               floor
                               pow
                               round])
            (towerdefense.field :refer [Target
                                        blocked?
                                        find-path
                                        find-path-on-map
                                        in-target?
                                        make-path-map
                                        pixel->tile])))

(defrecord Creep [creep-type health max-health value boss? coords target])

(defn make-creep [creep-type wave boss? coords target]
  (let [health (if boss?
                 (* 100 wave)
                 (* 10 wave))
        value (if boss?
                (* 10 wave)
                wave)]
    (Creep. creep-type
            health
            health
            value
            boss?
            coords
            target)))

(def ^:private creep-colors {:normal "silver"
                             :group "blue"
                             :fast "red"
                             :immune "fuchsia"
                             :flying "yellow"
                             :split "lime"
                             :dark "black"})

(defn creep-color [creep]
  (get creep-colors (:creep-type creep) "white"))

(def ^:private creep-speeds {:normal 4
                             :group 4
                             :fast 8
                             :immune 4
                             :flying 5
                             :split 3
                             :dark 2})

;; Tiles per second
(defn- creep-speed [creep]
  (let [base-speed (get creep-speeds (:creep-type creep) 4)]
    (if (:boss? creep)
      (* base-speed 0.8)
      base-speed)))

(defn- get-delta [state path-map creep]
  (let [[x y :as coords] (mapv floor (:coords creep))
        path (find-path-on-map path-map coords)
        [nx ny] (first path)]
    [(- nx x) (- ny y)]))

(defn- make-targets [[x y] dist [dx dy]]
  [[(+ x (* dx dist)) (+ y (* dy dist))]
   [x (+ y (* dy dist))]
   [(+ x (* dx dist)) y]
   [x y]])

(defn- move-creep [state path-map creep tick-seconds]
  (let [coords (:coords creep)
        dist (* (creep-speed creep) tick-seconds) ; how many pixels this moves
        delta (get-delta state path-map creep)
        possible-targets (remove #(blocked? state (mapv floor %)) (make-targets coords dist delta))]
    (assoc creep :coords (first possible-targets))))

(defn- dead? [creep]
  (<= (:health creep) 0))

(defn- finished? [creep]
  (and (in-target? (:target creep) (mapv floor (:coords creep)))
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

;; Remove creeps that are at their target
(defn- remove-creeps [state]
  (let [creeps (:creeps state)
        dead-creeps (filter #(dead? (second %)) creeps)
        dead-creeps-value (apply + (map #(get-in % [1 :value])
                                        dead-creeps))
        finished-creeps (filter #(finished? (second %)) creeps)]
    (-> state
        (update :score + dead-creeps-value)
        (update :money + dead-creeps-value)
        (update :lives - (count finished-creeps))
        (update :creeps (fn [creeps]
                          (apply dissoc creeps (map first dead-creeps))))
        (update :creeps (fn [creeps]
                          (apply dissoc creeps (map first finished-creeps)))))))

(defn update-creeps [state tick-time]
  (let [tick-seconds (/ tick-time 1000)]
    (-> state
        (move-creeps tick-seconds)
        remove-creeps)))
