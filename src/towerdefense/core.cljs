(ns towerdefense.core
  (:require (towerdefense.render :refer (render-game))
            (towerdefense.tower :refer (Tower make-tower))
            (towerdefense.creep :refer (Creep make-creep update-creeps))))

;; This is mostly for testing purposes
(defn add-random-tower [state]
  (let [tower (make-tower (rand-nth [:pellet :squirt :dart])
                          (inc (rand-int 9))
                          (rand-int 49)
                          (rand-int 39))]
    (update state :towers conj tower)))

;; And so is this...
(defn maybe-add-random-tower [state]
  (if (zero? (mod (:frames-rendered state) 100))
    (add-random-tower state)
    state))

(defn add-random-creep [state]
  (let [creep (make-creep (rand-nth [:normal :fast :immune :group])
                          (inc (rand-int 50))
                          0
                          (rand-int 600))]
    (update state :creeps conj creep)))

(defn maybe-add-random-creep [state]
  (if (zero? (rand-int 50))
    (add-random-creep state)
    state))

(defn update-state [state tick-time]
  (-> state
      maybe-add-random-tower
      maybe-add-random-creep
      (update-creeps tick-time)
      (update :frames-rendered inc)))

(defn frame-callback [state old-timestamp]
  (fn [new-timestamp]
    (let [new-state (update-state state (- new-timestamp old-timestamp))]
      (render-game new-state)
      (.requestAnimationFrame js/window
                              (frame-callback new-state new-timestamp)))))

(defn new-game []
  {:frames-rendered 0
   :towers []
   :creeps []
   :money 100
   :lives 20})

(defn start-game [timestamp]
  (.requestAnimationFrame js/window (frame-callback (new-game) timestamp)))

(.requestAnimationFrame js/window start-game)
