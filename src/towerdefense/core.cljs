(ns towerdefense.core
  (:require (towerdefense.creep :refer [Creep
                                        make-creep
                                        update-creeps])
            (towerdefense.field :refer [Target
                                        make-target])
            (towerdefense.input :refer [init-input
                                        process-inputs])
            (towerdefense.render :refer [render-game])
            (towerdefense.tower :refer [Tower
                                        make-tower])))

;; This is mostly for testing purposes
(defn add-random-tower [state]
  (let [tower (make-tower (rand-nth [:pellet :squirt :dart])
                          (inc (rand-int 9))
                          (+ 10 (rand-int 30))
                          (rand-int 39))]
    (update state :towers conj tower)))

;; And so is this...
(defn maybe-add-random-tower [state]
  (if (zero? (mod (:frames-rendered state) 30))
    (add-random-tower state)
    state))

(defn add-random-creep [state]
  (let [creep (make-creep (rand-nth [:normal :fast :immune :group])
                          (inc (rand-int 50))
                          [0 (rand-int 640)]
                          (rand-nth (:targets state)))]
    (update state :creeps conj creep)))

(defn maybe-add-random-creep [state]
  (if (zero? (rand-int 150))
    (add-random-creep state)
    state))

(defn update-state [state tick-time]
  (-> state
      process-inputs
      maybe-add-random-creep
      (update-creeps tick-time)
      (update :frames-rendered inc)))

(defn frame-callback [state old-timestamp]
  (fn [new-timestamp]
    (let [new-state (update-state state (- new-timestamp old-timestamp))]
      (render-game new-state)
      (.requestAnimationFrame js/window
                              (frame-callback new-state new-timestamp)))))

(def initial-state
  {:frames-rendered 0
   :towers []
   :creeps []
   :money 100
   :lives 20
   :targets [(make-target [[49 18] [49 19] [49 20] [49 21]])]})

(defn start-game [timestamp]
  (.requestAnimationFrame js/window (frame-callback initial-state timestamp)))

(init-input)
(.requestAnimationFrame js/window start-game)
