;; This code is full of side effects and other nasty stuff, be careful

(ns towerdefense.input
  (:require (cljs.math :refer [round])
            (towerdefense.tower :refer [Tower
                                        make-tower])))

(def ^:private pressed-keys (atom []))

(defn- key-down-handler [event]
  (swap! pressed-keys conj (.-key event)))

(def ^:private mouse-pos (atom [0 0]))

(defn- mouse-move-handler [event]
  (reset! mouse-pos [(.-offsetX event) (.-offsetY event)]))

(def ^:private mouse-clicked (atom false))

(defn- mouse-down-handler [event]
  (reset! mouse-clicked true))

;; Initialize event listeners etc
(defn init-input []
  (let [canvas (.getElementById js/document "game-canvas")]
    (.addEventListener js/document "keydown" key-down-handler)
    (.addEventListener canvas "mousemove" mouse-move-handler)
    (.addEventListener canvas "mousedown" mouse-down-handler)))

(defn- process-pressed-key [state keycode]
  (case keycode
    "1" (assoc state :tower-to-build :pellet)
    "2" (assoc state :tower-to-build :squirt)
    "3" (assoc state :tower-to-build :dart)
    :else state))

(defn- process-pressed-keys [state]
  (let [old-keys (deref pressed-keys)]
    (reset! pressed-keys [])
    (reduce process-pressed-key state old-keys)))

(defn- try-build-tower [state]
  (let [[x y as pos] (:mouse-pos state)
        tower (make-tower (:tower-to-build state)
                          1
                          (dec (round (/ x 16)))
                          (dec (round (/ y 16))))]
    (update state :towers conj tower)))

(defn- process-mouse-clicks [state]
  (let [clicked? (deref mouse-clicked)]
    (reset! mouse-clicked false)
    (if-not clicked?
      state
      (try-build-tower state))))

(defn- process-mouse-events [state]
  (-> state
      (assoc :mouse-pos (deref mouse-pos))
      process-mouse-clicks))

;; Process inputs collected since last frame
(defn process-inputs [state]
  (-> state
      process-pressed-keys
      process-mouse-events))
