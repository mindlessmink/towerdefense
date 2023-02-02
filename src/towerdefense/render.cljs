(ns towerdefense.render
  (:require (cljs.math :refer [PI
                               round])
            (towerdefense.creep :refer [Creep
                                        creep-color])
            (towerdefense.field :refer [Target
                                        pixel->tile])
            (towerdefense.tower :refer [Tower
                                        tower-cost])))

(def tile-size 16)
(def tower-size (* 2 tile-size))

(defn- get-2d-context [canvas-name]
  (.getContext (.getElementById js/document canvas-name) "2d"))

(defn- draw-towers [context state]
  (doseq [tower (:towers state)]
    (let [x (* tile-size (:x tower))
          y (* tile-size (:y tower))]
      (set! (.-fillStyle context) "black")
      (.fillRect context x y tower-size tower-size)
      (set! (.-fillStyle context) "white")
      (set! (.-font context) "16px sans")
      (let [letter (-> (:tower-type tower)
                       name
                       first
                       .toUpperCase)
            level (:level tower)]
        (.fillText context (str letter level) (+ 2 x) (+ tile-size y))))))

(defn- draw-targets [context state]
  (doseq [target (:targets state)]
    (doseq [tile (:tiles target)]
      (let [x (* tile-size (first tile))
            y (* tile-size (second tile))]
        (set! (.-fillStyle context) "red")
        (.fillRect context x y tile-size tile-size)))))

(defn- draw-creeps [context state]
  (doseq [creep (:creeps state)]
    (let [size (if (:boss? creep)
                 (* tile-size 0.75)
                 (* tile-size 0.5))
          [x y] (mapv #(* tile-size %) (:coords creep))]
      (set! (.-fillStyle context) (creep-color creep))
      (.beginPath context)
      (.arc context
            x
            y
            size
            0
            (* 2 PI)
            true)
      (.fill context))))

(defn- draw-tower-to-build [context state]
  (set! (.-fillStyle context) "green")
  (let [[mouse-x mouse-y :as mouse-pos] (:mouse-pos state)
        [middle-x middle-y] [(round (/ mouse-x tile-size))
                             (round (/ mouse-y tile-size))]]
    (.fillRect context
               (- (* middle-x tile-size) tile-size)
               (- (* middle-y tile-size) tile-size)
               tower-size tower-size)))

(defn- draw-game-canvas [state]
  (let [context (get-2d-context "game-canvas")]
    (set! (.-fillStyle context) "#ddddaa")
    (.fillRect context 0 0 640 480)
    (draw-towers context state)
    (draw-targets context state)
    (draw-creeps context state)
    (draw-tower-to-build context state)))

(defn- draw-side-panel [state]
  (let [context (get-2d-context "side-panel")]
    (set! (.-fillStyle context) "#77ff77")
    (.fillRect context 0 0 200 480)
    (set! (.-fillStyle context) "black")
    (set! (.-font context) "16px sans")
    (.fillText context (str "Money: $" (:money state)) 0 20)
    (.fillText context (str "Lives: " (:lives state)) 0 50)
    (.fillText context
               (str "Selected tower: "
                    (name (get state :tower-to-build ""))
                    " ($"
                    (tower-cost (get state :tower-to-build))
                    ")")
               0 80)
    (.fillText context (str "Frame: " (:frames-rendered state)) 0 450)))

(defn render-game [state]
  (draw-game-canvas state)
  (draw-side-panel state))
