(ns towerdefense.core
  (:require (towerdefense.creep :refer [Creep
                                        make-creep
                                        update-creeps])
            (towerdefense.field :refer [Target
                                        make-path-map
                                        make-target])
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
  (if (zero? (rand-int (* 10 (inc (count (:towers state))))))
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

(defn maybe-update-path-map [state]
  (let [path-map (:path-map state)
        old-towers (:old-towers state)]
    (if (or (nil? path-map) (not= old-towers (:towers state)))
      (assoc state
             :path-map (make-path-map state (:tiles (first (:targets state))))
             :old-towers (:towers state))
      state)))

(defn update-state [state tick-time]
  (-> state
      maybe-add-random-tower
      maybe-add-random-creep
      maybe-update-path-map
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
   :targets [(make-target [[49 18] [49 19] [49 20] [49 21]])]
   :path-map nil})

(defn start-game [timestamp]
  (.requestAnimationFrame js/window (frame-callback initial-state timestamp)))

(.requestAnimationFrame js/window start-game)
