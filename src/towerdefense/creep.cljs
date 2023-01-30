(ns towerdefense.creep
  (:require (cljs.math :refer [ceil
                               pow
                               round])
            (towerdefense.field :refer [Target
                                        blocked?
                                        find-path
                                        find-path-on-map
                                        in-target?
                                        make-path-map
                                        pixel->tile])))

(defrecord Creep [creep-type health max-health value boss? coords target id])

(defn make-creep [creep-type wave coords target]
  (let [health (round (* 10 (pow 1.1 wave)))
        value (ceil (/ wave 8))]
    (Creep. creep-type
            health
            health
            value
            (zero? (mod wave 8))
            coords
            target
            (gensym (str (name creep-type) "creep")))))

(def ^:private creep-colors {:normal "silver"
                             :group "blue"
                             :fast "red"
                             :immune "fuchsia"
                             :flying "yellow"
                             :split "lime"
                             :dark "black"})

(defn creep-color [creep]
  (get creep-colors (:creep-type creep) "white"))

(def ^:private creep-speeds {:normal 50
                             :group 50
                             :fast 100
                             :immune 50
                             :flying 60
                             :split 40
                             :dark 25})

;; Pixels per second
(defn- creep-speed [creep]
  (let [base-speed (get creep-speeds (:creep-type creep) 50)]
    (if (:boss? creep)
      (* base-speed 0.8)
      base-speed)))

(defn- get-delta [state path-map creep]
  (let [[x y :as coords] (pixel->tile (:coords creep))
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
        possible-targets (remove #(blocked? state (pixel->tile %)) (make-targets coords dist delta))]
    (assoc creep :coords (first possible-targets))))

(defn- finished? [creep]
  (in-target? (:target creep) (pixel->tile (:coords creep))))

(defn- move-creeps [state tick-seconds]
  (let [creeps (:creeps state)
        ;; FIXME: assumes all creeps have the same target
        path-map (:path-map state)]
    (assoc state :creeps (mapv #(move-creep state path-map % tick-seconds) creeps))))

;; Remove creeps that are at their target
(defn- remove-creeps [state]
  (let [creeps (:creeps state)
        finished-creeps (filter finished? creeps)]
    (-> state
        (update :lives - (count finished-creeps))
        (update :creeps #(filterv (complement finished?) %)))))

(defn update-creeps [state tick-time]
  (let [tick-seconds (/ tick-time 1000)]
    (-> state
        (move-creeps tick-seconds)
        remove-creeps)))
