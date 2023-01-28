(ns towerdefense.render)

(defn render-game [state]
  (let [canvas (.getElementById js/document "game-canvas")
        context (.getContext canvas "2d")]
    (set! (.-fillStyle context) "#ddddaa")
    (.fillRect context 0 0 800 600)
    (set! (.-fillStyle context) "black")
    (set! (.-font context) "16px sans")
    (.fillText context (str (:frames-rendered state) " frames rendered") 20 20)))
