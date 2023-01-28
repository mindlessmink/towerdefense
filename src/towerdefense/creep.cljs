(ns towerdefense.creep
  (:require (cljs.math :refer (ceil pow round))))

(defrecord Creep [creep-type health max-health value x y id])

(defn make-creep [creep-type wave x y]
  (let [health (round (* 10 (pow 1.1 wave)))
        value (ceil (/ wave 8))]
    (Creep. creep-type
            health
            health
            value
            x
            y
            (gensym (str (name creep-type) "creep")))))

(def creep-colors {:normal "silver"
                   :group "blue"
                   :fast "red"
                   :immune "fuchsia"
                   :flying "yellow"
                   :split "lime"
                   :dark "black"})

(defn creep-color [creep]
  (get creep-colors (:creep-type creep) "white"))

;; Pixels per second
(defn- creep-speed [creep]
  (if (= (:creep-type creep) :fast)
    100
    50))

(defn- move-creep [creep tick-seconds]
  (update creep :x + (* (creep-speed creep) tick-seconds)))

(defn update-creeps [state tick-time]
  (let [tick-seconds (/ tick-time 1000)]
    (assoc state :creeps (->> (:creeps state)
                              (map #(move-creep % tick-seconds))
                              (remove #(>= (:x %) 800))
                              vec))))
