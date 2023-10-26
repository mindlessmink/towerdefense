(ns towerdefense.core
  (:require ("react" :as react)
            (reagent.core :as r)
            (reagent.dom :as rd)
            (towerdefense.projectile :refer [update-projectiles])
            (towerdefense.common :refer [game-over?])
            (towerdefense.creep :refer [Creep
                                        make-creep
                                        update-creeps])
            (towerdefense.field :refer [Target
                                        make-blockmap
                                        make-path-map
                                        make-target
                                        maybe-update-path-maps])
            (towerdefense.init :refer [make-initial-state])
            (towerdefense.input :refer [init-input
                                        process-inputs])
            (towerdefense.render :refer [render-game])
            (towerdefense.spawner :refer [update-spawners])
            (towerdefense.tower :refer [Tower
                                        make-tower
                                        update-towers])))

(defn maybe-game-over [state]
  (when (game-over? state)
    ((:game-over-fn state) state))
  state)

(defn update-state [state tick-time]
  (let [tick-seconds (/ tick-time 1000)
        state (process-inputs state)]
    (if (game-over? state)
      state
      (-> state
          process-inputs
          maybe-update-path-maps
          (update-spawners tick-seconds)
          (update-towers tick-seconds)
          (update-projectiles tick-seconds)
          (update-creeps tick-seconds)
          maybe-game-over
          (update :frames-rendered inc)))))

(defn frame-callback [canvas state old-timestamp]
  (fn [new-timestamp]
    (let [new-state (update-state state (- new-timestamp old-timestamp))]
      (render-game canvas new-state)
      (.requestAnimationFrame js/window
                              (frame-callback canvas new-state new-timestamp)))))

(defn start-game [canvas game-over-fn]
  (fn [timestamp]
    (.requestAnimationFrame js/window (frame-callback canvas
                                                      (make-initial-state game-over-fn)
                                                      timestamp))))

(defn td-canvas [canvas-ref]
  [:div
   [:canvas {:id "game-canvas", :width 800, :height 480, :ref canvas-ref}]])

(def all-scores (r/atom []))

(defn record-score
  "Add a new score to the score list"
  [state]
  (let [num-scores (count @all-scores)]
    (swap! all-scores conj {:id (inc num-scores), :score (:score state)})))

(defn td-high-scores []
  (let [scores @all-scores
        top-ten (take 10 (sort-by :score > scores))]
    [:div
     [:p "Top 10 scores:"]
     [:ul
      (for [entry top-ten]
        ^{:key (:id entry)} [:li (:score entry)])]]))


(defn td-game []
  (let [canvas-ref (react/useRef nil)]
    (react/useEffect (fn []
                       (init-input (.-current canvas-ref))
                       (.requestAnimationFrame js/window (start-game (.-current canvas-ref) record-score))
                       js/undefined))
    [:div
     [td-canvas canvas-ref]
     [td-high-scores]
     [:p "Tower Defense game, version 0.3"]]))

(defn ^:dev/after-load start []
  (rd/render [:f> td-game] (.getElementById js/document "app")))

(defn init []
  (start))
