(ns towerdefense.render
  (:require (cljs.math :refer [PI
                               ceil
                               round])
            (towerdefense.creep :refer [Creep
                                        creep-color])
            (towerdefense.field :refer [Target
                                        find-targets
                                        pixel->tile])
            (towerdefense.spawner :refer [describe-wave
                                          wave-time])
            (towerdefense.tower :refer [Tower
                                        tower-build-cost
                                        upgradeable?
                                        upgrade-cost])))

(def tile-size 16)
(def tower-size (* 2 tile-size))

(defn- draw-circle [context x y radius]
  (.beginPath context)
  (.arc context x y radius 0 (* 2 PI) true)
  (.fill context))

(defn- get-2d-context [canvas-name]
  (.getContext (.getElementById js/document canvas-name) "2d"))

(defn- draw-towers [context state]
  (let [selected-tower (:selected-tower state)]
    (doseq [[id tower] (:towers state)]
      (let [x (* tile-size (:x tower))
            y (* tile-size (:y tower))]
        (if (= id selected-tower)
          (set! (.-fillStyle context) "#000088")
          (set! (.-fillStyle context) "black"))
        (.fillRect context x y tower-size tower-size)
        (set! (.-fillStyle context) "white")
        (set! (.-font context) "16px sans")
        (let [letter (-> (:tower-type tower)
                         name
                         first
                         .toUpperCase)
              level (:level tower)]
          (.fillText context (str letter level) (+ 2 x) (+ tile-size y)))))))

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

(defn- draw-creeps [context state]
  (doseq [[id creep] (:creeps state)]
    (let [size (creep-size creep)
          [x y] (mapv #(* tile-size %) (:coords creep))]
      (when (:frosted creep)
        (set! (.-fillStyle context) "#00dddd")
        (draw-circle context x y (+ 2 size)))
      (set! (.-fillStyle context) (creep-color creep))
      (draw-circle context x y size)
      (draw-creep-health-bar context creep))))

(defn- draw-projectiles [context state]
  (doseq [projectile (:projectiles state)]
    (let [[x y] (mapv #(* tile-size %) (:coords projectile))]
      (set! (.-fillStyle context) "blue")
      (draw-circle context x y 2))))

(defn- draw-tower-to-build [context state]
  (when (:tower-to-build state)
    (set! (.-fillStyle context) "green")
    (let [[mouse-x mouse-y :as mouse-pos] (:mouse-pos state)
          [middle-x middle-y] [(round (/ mouse-x tile-size))
                               (round (/ mouse-y tile-size))]]
      (.fillRect context
                 (- (* middle-x tile-size) tile-size)
                 (- (* middle-y tile-size) tile-size)
                 tower-size tower-size))))

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
    (draw-tower-to-build context state)))

(defn- draw-selected-tower [context state]
  (when-let [tower (get (:towers state) (:selected-tower state))]
    (.fillText context
               (str "Selected tower: " (name (:tower-type tower)))
               0 100)
    (when (upgradeable? tower)
      (.fillText context
                 (str "Upgrade cost: $" (upgrade-cost tower))
                 0 120))))

(defn- draw-wave-info [context state]
  (let [spawner (first (:spawners state))
        wave-num (:curr-wave-num spawner)
        next-wave (inc wave-num)]
    (if (zero? wave-num)
      (.fillText context "Press <n> to start" 0 200)
      (do
        (.fillText context (str "Current wave: " wave-num) 0 200)
        (.fillText context (describe-wave wave-num) 20 220)
        (if-let [timer (:time-since-last-wave spawner)]
          (let [time-remaining (ceil (- wave-time timer))]
            (.fillText context
                       (str "Next wave in " time-remaining " seconds")
                       0 240)
            (.fillText context (describe-wave next-wave) 20 260)))))))

(defn- draw-side-panel [state]
  (let [context (get-2d-context "side-panel")]
    (set! (.-fillStyle context) "#77ff77")
    (.fillRect context 0 0 200 480)
    (set! (.-fillStyle context) "black")
    (set! (.-font context) "16px sans")
    (.fillText context (str "Score: " (:score state)) 0 20)
    (.fillText context (str "Money: $" (:money state)) 0 40)
    (.fillText context (str "Lives: " (:lives state)) 0 60)
    (when-let [tower-to-build (:tower-to-build state)]
      (.fillText context
                 (str "Selected tower: "
                      (name tower-to-build)
                      " ($"
                      (tower-build-cost tower-to-build)
                      ")")
                 0 80))
    (draw-selected-tower context state)
    (draw-wave-info context state)
    (.fillText context (str "Frame: " (:frames-rendered state)) 0 450)))

(defn render-game [state]
  (draw-game-canvas state)
  (draw-side-panel state))
