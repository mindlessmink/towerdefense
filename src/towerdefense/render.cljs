(ns towerdefense.render
  (:require (cljs.math :refer [PI
                               ceil
                               round])
            (towerdefense.common :refer [game-over?])
            (towerdefense.creep :refer [Creep
                                        creep-color])
            (towerdefense.field :refer [Target
                                        find-targets
                                        pixel->tile])
            (towerdefense.spawner :refer [describe-wave
                                          wave-time])
            (towerdefense.tower :refer [Tower
                                        tower-build-cost
                                        tower-radius
                                        tower-radius-by-type
                                        upgradeable?
                                        upgrade-cost])))

;; Internally the game uses tiles to represent coordinates. One tile is
;; one fourth of a tower. These sizes also include the side panel.
(def canvas-width 50)
(def canvas-height 30)

;; Originally the game used a tile size of 16 pixels. We'll continue using
;; that for now but it will be scaled to the actual size.
(def tile-size 16)
(def tower-size (* 2 tile-size))

;;; The canvas is scaled as big as possible to fill the browser window.
(defn calculate-scale-factor []
  (let [window-width (.-innerWidth js/window)
        window-height (.-innerHeight js/window)
        canvas-ratio (/ canvas-width canvas-height)
        window-ratio (/ window-width window-height)]
    (if (> window-ratio canvas-ratio)
      (/ window-height canvas-height tile-size)
      (/ window-width canvas-width tile-size))))

(defn- draw-circle [context x y radius outline?]
  (.beginPath context)
  (.arc context x y radius 0 (* 2 PI) true)
  (.fill context)
  (when outline?
    (.stroke context)))

(defn- get-2d-context [canvas-name]
  (.getContext (.getElementById js/document canvas-name) "2d"))

(defn- draw-tower-at
  ([context tower-type [x y]] (draw-tower-at context tower-type [x y] 0 0 false))
  ([context tower-type [x y] angle level selected?]
   (.save context)
   (if selected?
     (set! (.-strokeStyle context) "#000088")
     (set! (.-strokeStyle context) "black"))
   (.translate context (+ x tile-size) (+ y tile-size))
   (.strokeRect context (- tile-size) (- tile-size) tower-size tower-size)
   ;; one dot for each upgrade
   (set! (.-fillStyle context) "blue")
   (loop [dots (dec level)
          x (* -0.875 tile-size)]
     (when (pos? dots)
       (.fillRect context x (* 0.7 tile-size) (* 0.25 tile-size) (* 0.25 tile-size))
       (recur (dec dots) (+ x (* 0.375 tile-size)))))
   ;; swarm tower doesn't rotate
   (when-not (= :swarm tower-type)
     (.rotate context (- (* 2 PI) angle)))
   (case tower-type
     :pellet (do
               (.beginPath context)
               (.arc context 0 0 (* 0.2 tile-size) 0 (* 2 PI))
               (.stroke context)
               (.strokeRect context (* 0.2 tile-size) (* -0.1 tile-size) (* 0.5 tile-size) (* 0.2 tile-size)))
     :squirt (do
               (.beginPath context)
               (.arc context (* -0.3 tile-size) 0 (* 0.6 tile-size) (* 0.2 PI) (* 1.8 PI))
               (.lineTo context (* 0.8 tile-size) (* -0.2 tile-size))
               (.lineTo context (* 0.8 tile-size) (* 0.2 tile-size))
               (.closePath context)
               (.stroke context))
     :dart (do
             (.strokeRect context (* -0.8 tile-size) (* -0.8 tile-size) (* 1.6 tile-size) (* 0.6 tile-size))
             (.strokeRect context (* -0.8 tile-size) (* 0.2 tile-size) (* 1.6 tile-size) (* 0.6 tile-size))
             (.strokeRect context (* -0.2 tile-size) (* -0.2 tile-size) (* 0.4 tile-size) (* 0.4 tile-size)))
     :swarm (do
              (.beginPath context)
              (.moveTo context (* -0.5 tile-size) 0)
              (.lineTo context 0 (* -0.5 tile-size))
              (.lineTo context (* 0.5 tile-size) 0)
              (.lineTo context 0 (* 0.5 tile-size))
              (.closePath context)
              (.stroke context))
     :frost (do
              (set! (.-fillStyle context) "white")
              (.beginPath context)
              (.arc context 0 0 (* 0.4 tile-size) (* 0.3 PI) (* 1.7 PI))
              (.lineTo context (* 0.6 tile-size) (* -0.3 tile-size))
              (.lineTo context (* 0.6 tile-size) (* 0.3 tile-size))
              (.closePath context)
              (.fill context)
              (.stroke context))
     nil)
   (.restore context)))

(defn- draw-towers [context state]
  (let [selected-tower (:selected-tower state)]
    (doseq [[id tower] (:towers state)]
      (let [x (* tile-size (:x tower))
            y (* tile-size (:y tower))
            angle (:dir tower)
            tower-type (:tower-type tower)
            level (:level tower)]
        (draw-tower-at context tower-type [x y] angle level (= selected-tower id))))))

(defn- draw-walls [context state]
  (set! (.-fillStyle context) "#bbbbbb")
  (doseq [[x y :as wall] (:walls state)]
    (.fillRect context
               (* x tile-size)
               (* y tile-size)
               tile-size
               tile-size)))

(defn- draw-targets [context state]
  (doseq [target (find-targets state)]
    (doseq [tile (:tiles target)]
      (let [x (* tile-size (first tile))
            y (* tile-size (second tile))]
        (set! (.-fillStyle context) "red")
        (.fillRect context x y tile-size tile-size)))))

(def ^:private delta->symbol
  {[-1 -1] "⇖"
   [ 0 -1] "↑"
   [ 1 -1] "⇗"
   [-1  0] "←"
   [ 1  0] "→"
   [-1  1] "⇙"
   [ 0  1] "↓"
   [ 1  1] "⇘"})

(defn- draw-path-map [context state]
  (set! (.-fillStyle context) "black")
  (set! (.-font context) "16px sans")
  (let [path-map (:path-map state)]
    (doseq [x (range 50)
            y (range 40)]
      (let [tile [x y]
            next-tile (get path-map tile nil)]
        (when-not (nil? next-tile)
          (let [delta [(- (first next-tile) x) (- (second next-tile) y)]
                symbol (get delta->symbol delta "x")]
            (.fillText context symbol (* 16 (inc x)) (* 16 (inc y)))))))))

(defn- draw-creep-health-bar [context creep]
  (let [health (:health creep)
        max-health (:max-health creep)
        ratio (/ health max-health)
        [x y] (mapv #(* tile-size %) (:coords creep))]
    (set! (.-fillStyle context) "red")
    (.fillRect context
               (- x 8)
               (+ y 8)
               16 2)
    (set! (.-fillStyle context) "green")
    (.fillRect context
               (- x 8)
               (+ y 8)
               (* 16 ratio)
               2)))

(defn- creep-size [creep]
  (cond
    (:boss? creep) (* tile-size 0.75)
    (= :spawn (:creep-type creep)) (case (get creep :spawn-level 0)
                                     0 (* tile-size 0.5)
                                     1 (* tile-size 0.4)
                                     2 (* tile-size 0.3)
                                     (* tile-size 0.2))
    :else (* tile-size 0.5)))

(defn- draw-creep-shape [context creep]
  (let [[x y] (mapv (partial * tile-size) (:coords creep))
        size (creep-size creep)
        dir (:dir creep)
        old-tf (.getTransform context)
        old-fs (.-fillStyle context)]
    (.translate context x y)
    (.rotate context (- (* 2 PI) dir))
    (set! (.-fillStyle context) (creep-color creep))
    (if (:frosted creep)
      (set! (.-strokeStyle context) "#00dddd")
      (set! (.-strokeStyle context) "black"))
    (case (:creep-type creep)
      :normal (draw-circle context 0 0 size true)
      :fast (do
              (.beginPath context)
              (.moveTo context (* -1.2 size) 0)
              (.lineTo context 0 (* 0.75 size))
              (.arc context 0 0 (* 0.75 size) (* 0.5 PI) (* 1.5 PI) true)
              (.closePath context)
              (.fill context)
              (.stroke context))
      :immune (draw-circle context 0 0 size true)
      :group (draw-circle context 0 0 size true)
      :spawn (if (contains? creep :spawn-level)
               (draw-circle context 0 0 size true)
               (do
                 (.beginPath context)
                 (.arc context (* 0.5 size) 0 (* 0.5 size) (* 1.2 PI) (* 0.8 PI))
                 (.arc context (* -0.5 size) 0 (* 0.5 size) (* 0.2 PI) (* 1.8 PI))
                 (.closePath context)
                 (.fill context)
                 (.stroke context)))
      :dark (do
              (.beginPath context)
              (.arc context 0 0 size (* 0.6 PI) (* 1.4 PI) true)
              (.lineTo context 0 0)
              (.closePath context)
              (.fill context)
              (.stroke context))
      :flying (do
                (.beginPath context)
                (.moveTo context (- size) (* -0.75 size))
                (.lineTo context (- size) (* 0.75 size))
                (.lineTo context size 0)
                (.lineTo context (- size) (* -0.75 size))
                (.fill context)
                (.stroke context))
      nil)
    (set! (.-fillStyle context) old-fs)
    (.setTransform context old-tf)))

(defn- draw-creeps [context state]
  (doseq [[id creep] (:creeps state)]
    (let [size (creep-size creep)
          [x y] (mapv #(* tile-size %) (:coords creep))]
      (draw-creep-shape context creep)
      (draw-creep-health-bar context creep))))

(defn- draw-projectiles [context state]
  (doseq [projectile (:projectiles state)]
    (let [[x y] (mapv #(* tile-size %) (:coords projectile))]
      (set! (.-fillStyle context) "blue")
      (draw-circle context x y 2 false))))

(defn- draw-tower-to-build [context state]
  (when (:tower-to-build state)
    (set! (.-fillStyle context) "green")
    (let [[mouse-x mouse-y :as mouse-pos] (:mouse-pos state)
          [middle-x middle-y] [(round (/ mouse-x tile-size))
                               (round (/ mouse-y tile-size))]]
      (.fillRect context
                 (- (* middle-x tile-size) tile-size)
                 (- (* middle-y tile-size) tile-size)
                 tower-size tower-size)
      (set! (.-fillStyle context) "#ffffff")
      (set! (.-globalAlpha context) "0.2")
      (draw-circle context
                   (* middle-x tile-size)
                   (* middle-y tile-size)
                   (* tile-size (tower-radius-by-type (:tower-to-build state)))
                   false)
      (set! (.-globalAlpha context) "1.0"))))

(defn- draw-selected-tower-radius [context state]
  (set! (.-fillStyle context) "#ffffff")
  (set! (.-globalAlpha context) "0.2")
  (when-let [selected-tower (:selected-tower state)]
    (let [tower (get-in state [:towers selected-tower])
          [x y] [(:x tower) (:y tower)]
          [x y] [(* tile-size (inc x)) (* tile-size (inc y))]]
      (draw-circle context x y (* tile-size (tower-radius tower)) false)))
  (set! (.-globalAlpha context) "1.0"))

(defn- draw-game-canvas [state]
  (let [context (get-2d-context "game-canvas")]
    (set! (.-fillStyle context) "#ddddaa")
    (.fillRect context 0 0 640 480)
    (draw-towers context state)
    (draw-walls context state)
    (draw-targets context state)
    (draw-creeps context state)
    (draw-projectiles context state)
    ; (draw-path-map context state)
    (draw-tower-to-build context state)
    (draw-selected-tower-radius context state)))

(defn- draw-selected-tower [context state]
  (when-let [tower (get (:towers state) (:selected-tower state))]
    (.fillText context
               (str "Selected tower: " (name (:tower-type tower)))
               640 160)
    (when (upgradeable? tower)
      (.fillText context
                 (str "Upgrade cost: $" (upgrade-cost tower))
                 640 180))))

(defn- draw-wave-info [context state]
  (let [spawner (first (:spawners state))
        wave-num (:curr-wave-num spawner)
        next-wave (inc wave-num)]
    (if (zero? wave-num)
      (.fillText context "Press <n> to start" 640 260)
      (do
        (.fillText context (str "Current wave: " wave-num) 640 260)
        (.fillText context (describe-wave wave-num) 660 280)
        (if-let [timer (:time-since-last-wave spawner)]
          (let [time-remaining (ceil (- wave-time timer))]
            (.fillText context
                       (str "Next wave in " time-remaining " seconds")
                       640 300)
            (.fillText context (describe-wave next-wave) 660 320)))))))

(defn- draw-side-panel [state]
  (let [context (get-2d-context "game-canvas")]
    (set! (.-fillStyle context) "#77ff77")
    (.fillRect context 640 0 160 480)
    (set! (.-fillStyle context) "black")
    (set! (.-font context) "16px sans")
    (.fillText context (str "Score: " (:score state)) 640 20)
    (.fillText context (str "Money: $" (:money state)) 640 40)
    (.fillText context (str "Lives: " (:lives state)) 640 60)
    ;; tower type selection buttons
    (draw-tower-at context :pellet [640 80])
    (draw-tower-at context :squirt [(+ 640 tower-size) 80])
    (draw-tower-at context :dart [(+ 640 (* 2 tower-size)) 80])
    (draw-tower-at context :swarm [(+ 640 (* 3 tower-size)) 80])
    (draw-tower-at context :frost [(+ 640 (* 4 tower-size)) 80])
    (when-let [tower-to-build (:tower-to-build state)]
      (.fillText context "Selected tower:" 640 140)
      (.fillText context
                 (str (name tower-to-build)
                      " ($"
                      (tower-build-cost tower-to-build)
                      ")")
                 660 160))
    (draw-selected-tower context state)
    (draw-wave-info context state)
    (.fillText context (str "Frame: " (:frames-rendered state)) 640 450)))

(defn- draw-game-over-screen [state]
  (let [context (get-2d-context "game-canvas")]
    (set! (.-fillStyle context) "#000000")
    (set! (.-globalAlpha context) "0.5")
    (.fillRect context 0 0 640 480)
    (set! (.-globalAlpha context) "1.0")
    (set! (.-fillStyle context) "#ffffff")
    (set! (.-font context) "32px sans")
    (.fillText context
               (str "Game over!\nFinal score: "
                    (:score state))
               100 200)))

(defn render-game [canvas state]
  (let [game-canvas (.getElementById js/document "game-canvas")
        context (.getContext game-canvas "2d")
        scale-factor (calculate-scale-factor)]
    (set! (.-width game-canvas) (* canvas-width tile-size scale-factor))
    (set! (.-height game-canvas) (* canvas-height tile-size scale-factor))
    (.resetTransform context)
    (.scale context scale-factor scale-factor))
  (draw-game-canvas state)
  (draw-side-panel state)
  (if (game-over? state)
    (draw-game-over-screen state)))
