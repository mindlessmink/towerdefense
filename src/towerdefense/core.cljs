(ns towerdefense.core
  (:require (towerdefense.creep :refer [Creep
                                        make-creep
                                        update-creeps])
            (towerdefense.field :refer [Target
                                        make-path-map
                                        make-target])
            (towerdefense.input :refer [init-input
                                        process-inputs])
            (towerdefense.render :refer [render-game])
            (towerdefense.tower :refer [Tower
                                        make-tower])))

(defn add-random-creep [state]
  (let [creep (make-creep (rand-nth [:normal :fast :immune :group])
                          (inc (rand-int 50))
                          [0 (rand-int 30)]
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
      process-inputs
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
   :money 200
   :lives 20
   :targets [(make-target [[39 13] [39 14] [39 15] [39 16]])]
   :path-map nil})

(defn start-game [timestamp]
  (.requestAnimationFrame js/window (frame-callback initial-state timestamp)))

(init-input)
(.requestAnimationFrame js/window start-game)
