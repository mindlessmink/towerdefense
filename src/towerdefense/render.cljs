(ns towerdefense.render
  (:require (cljs.math :refer (PI))
            (towerdefense.tower :refer (Tower))
            (towerdefense.creep :refer (Creep creep-color))))

(defn- get-2d-context [canvas-name]
  (.getContext (.getElementById js/document canvas-name) "2d"))

(defn- draw-towers [context state]
  (doseq [tower (:towers state)]
    (let [x (* 16 (:x tower))
          y (* 16 (:y tower))]
      (set! (.-fillStyle context) "black")
      (.fillRect context x y 32 32)
      (set! (.-fillStyle context) "white")
      (set! (.-font context) "16px sans")
      (let [letter (-> (:tower-type tower)
                       name
                       first
                       .toUpperCase)
            level (:level tower)]
        (.fillText context (str letter level) (+ 2 x) (+ 16 y))))))

(defn- draw-creeps [context state]
  (doseq [creep (:creeps state)]
    (set! (.-fillStyle context) (creep-color creep))
    (.beginPath context)
    (.arc context
          (+ 8 (:x creep))
          (+ 8 (:y creep))
          8
          0
          (* 2 PI)
          true)
    (.fill context)))

(defn- draw-game-canvas [state]
  (let [context (get-2d-context "game-canvas")]
    (set! (.-fillStyle context) "#ddddaa")
    (.fillRect context 0 0 800 600)
    (draw-towers context state)
    (draw-creeps context state)))

(defn- draw-side-panel [state]
  (let [context (get-2d-context "side-panel")]
    (set! (.-fillStyle context) "#77ff77")
    (.fillRect context 0 0 200 600)
    (set! (.-fillStyle context) "black")
    (set! (.-font context) "16px sans")
    (.fillText context (str "Frame: " (:frames-rendered state)) 0 20)))

(defn render-game [state]
  (draw-game-canvas state)
  (draw-side-panel state))
