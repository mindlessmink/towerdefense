(ns towerdefense.creep
  (:require (cljs.math :refer (ceil pow round))))

(defrecord Creep [creep-type health max-health value boss? x y id])

(defn make-creep [creep-type wave x y]
  (let [health (round (* 10 (pow 1.1 wave)))
        value (ceil (/ wave 8))]
    (Creep. creep-type
            health
            health
            value
            (zero? (mod wave 8))
            x
            y
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

(defn- move-creep [creep tick-seconds]
  (update creep :x + (* (creep-speed creep) tick-seconds)))

(defn- finished? [creep]
  (>= (:x creep) 800))

(defn- move-creeps [state tick-seconds]
  (let [creeps (:creeps state)]
    (assoc state :creeps (mapv #(move-creep % tick-seconds) creeps))))

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
