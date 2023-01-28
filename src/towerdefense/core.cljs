(ns towerdefense.core)

(let [canvas (.getElementById js/document "game-canvas")
      ctx (.getContext canvas "2d")]
  (set! (.-font ctx) "16px sans")
  (.fillText ctx "Tower defense test!!" 20 20))
