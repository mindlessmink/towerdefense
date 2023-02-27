;; This code is full of side effects and other nasty stuff, be careful

(ns towerdefense.input
  (:require (cljs.math :refer [floor
                               round])
            (towerdefense.field :refer [make-blockmap
                                        maybe-update-path-maps
                                        tower-tiles])
            (towerdefense.tower :refer [Tower
                                        make-tower
                                        tower-build-cost
                                        tower-cost
                                        upgradeable?
                                        upgrade-cost
                                        upgrade-tower])))

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

(defn- try-sell-tower [state]
  (let [selected-tower (:selected-tower state)]
    (if-not selected-tower
      state
      (let [tower (get (:towers state) selected-tower)]
        (-> state
            (update :towers dissoc selected-tower)
            (update :money + (floor (* (tower-cost tower) 0.8)))
            (dissoc :selected-tower))))))

(defn- try-upgrade-tower [state]
  (if-let [tower-id (:selected-tower state)]
    (let [towers (:towers state)
          tower (get towers tower-id)]
      (if-not (upgradeable? tower)
        state
        (let [cost (upgrade-cost tower)]
          (if (>= (:money state) cost)
            (assoc state
                   :towers (update towers tower-id upgrade-tower)
                   :money (- (:money state) cost))
            state))))
    state))

(defn- start-spawners [state]
  (assoc state
         :spawners (mapv (fn [spawner]
                           ;; ugly
                           (assoc spawner :time-since-last-wave 10000))
                         (:spawners state))))

(defn- process-pressed-key [state keycode]
  (case keycode
    "1" (assoc state :tower-to-build :pellet)
    "2" (assoc state :tower-to-build :squirt)
    "3" (assoc state :tower-to-build :dart)
    "5" (assoc state :tower-to-build :frost)
    "m" (update state :money + 10000) ; for testing
    "n" (start-spawners state)
    "s" (try-sell-tower state)
    "u" (try-upgrade-tower state)
    :else state))

(defn- process-pressed-keys [state]
  (let [old-keys (deref pressed-keys)]
    (reset! pressed-keys [])
    (reduce process-pressed-key state old-keys)))

(defn- path-maps-ok? [state]
  (let [path-maps (:path-maps state)
        spawners (:spawners state)]
    (every? (fn [spawner]
              (let [start-tiles (:start-area spawner)
                    path-map (get path-maps (:target spawner))]
                (every? #(contains? path-map %) start-tiles)))
            spawners)))

(defn- try-place-tower [tower cost state]
  (let [updated-state (-> state
                          (update :towers assoc (gensym "tower") tower)
                          (update :money - cost)
                          maybe-update-path-maps)]
    (if (path-maps-ok? updated-state)
      updated-state
      state)))

(defn- try-build-tower [state]
  (let [[x y :as pos] (:mouse-pos state)
        tower-to-build (:tower-to-build state)
        cost (tower-build-cost tower-to-build)
        money (:money state)]
    (if (or (nil? tower-to-build)
            (< money cost))
      state
      (let [tower (make-tower tower-to-build
                              1
                              (dec (round (/ x 16)))
                              (dec (round (/ y 16))))
            tiles (tower-tiles tower)
            blockmap (make-blockmap state)]
        (if (not-any? #(contains? blockmap %) tiles)
          (try-place-tower tower cost state)
          state)))))

(defn- process-mouse-clicks [state]
  (let [clicked? (deref mouse-clicked)
        [x y :as pos] (:mouse-pos state)
        [tx ty :as clicked-tile] [(floor (/ x 16))
                                  (floor (/ y 16))]
        towers (:towers state)]
    (reset! mouse-clicked false)
    (if-not clicked?
      state
      (if-let [tower (first (filter (fn [[id tower]]
                                      (some #(= clicked-tile %)
                                            (tower-tiles tower)))
                                    towers))]
        (-> state
            (assoc :selected-tower (first tower))
            (dissoc :tower-to-build))
        (-> (try-build-tower state)
            (dissoc :selected-tower))))))

(defn- process-mouse-events [state]
  (-> state
      (assoc :mouse-pos (deref mouse-pos))
      process-mouse-clicks))

;; Process inputs collected since last frame
(defn process-inputs [state]
  (-> state
      process-pressed-keys
      process-mouse-events))
