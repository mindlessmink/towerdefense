(ns towerdefense.field
  (:require (cljs.math :refer [floor])))

;; Creeps try to reach their target.
(defrecord Target [tiles])

(defn make-target [tiles]
  (Target. (apply hash-set tiles)))

(defn in-target? [target tile]
  (contains? (:tiles target) tile))

(defn- tower-tiles [tower]
  (let [x (:x tower)
        y (:y tower)]
    (hash-set [x y]
              [(inc x) y]
              [x (inc y)]
              [(inc x) (inc y)])))

;; Clojurescript doesn't have union??
(defn merge-sets [a b]
  (reduce conj a b))

(defn- make-tower-blockmap [towers]
  (reduce merge-sets (hash-set) (map tower-tiles towers)))

;; debugging...
(defn- tile-str [tile]
  (str "(" (first tile) ", " (second tile) ")"))

;; Set of tiles that are blocked (by towers etc)
(defn- make-blockmap [state]
  (make-tower-blockmap (:towers state)))

(defn blocked? [state tile]
  (contains? (make-blockmap state) tile))

(defn pixel->tile [[x y]]
  [(floor (/ x 16)) (floor (/ y 16))])

(defn- in-game-area? [tile]
  (and (<= 0 (:first tile) 49)
       (<= 0 (:second tile) 39)))

(defn- neighbors [tile]
  (map (fn [x]
         [(+ (first tile) (first x))
          (+ (second tile) (second x))])
       [[-1 -1] [0 -1] [1 -1]
        [-1  0]        [1  0]
        [-1  1] [0  1] [1  1]]))

(defn- valid-neighbors [tile blockmap]
  (->> (neighbors tile)
       (filter in-game-area?)
       (remove #(contains? blockmap %))))

(defn- estimate-cost [start-tile goal-tile]
  (+ (abs (- (first start-tile) (first goal-tile)))
     (abs (- (second start-tile) (second goal-tile)))))

(defn- estimate-cost-to-target [start-tile goal-tiles]
  (apply min (map #(estimate-cost start-tile %) goal-tiles)))

(defn- calculate-neighbor-costs [curr-cost neighbor-tiles blockmap]
  (map (fn [tile]
         [tile
          (if (contains? blockmap tile)
            (+ curr-cost 10000)
            (+ curr-cost 1))])
       neighbor-tiles))

(defn- build-path-helper [curr-tile previous path]
  (if-not (contains? previous curr-tile)
    path
    (build-path-helper (get previous curr-tile)
                       previous
                       (conj path curr-tile))))

(defn- build-path [curr-tile previous]
  (build-path-helper curr-tile previous '()))

(defn find-path [state start-tile goal-tiles]
  (let [blockmap (make-blockmap state)]
    (loop [open-set (sorted-set [(estimate-cost-to-target start-tile goal-tiles) start-tile])
           previous (hash-map)
           cheapest (hash-map start-tile 0)]
      (if (empty? open-set)
        nil ;; mission failed
        (let [[_ curr :as item] (first open-set)
              remaining (disj open-set item)
              curr-cost (get cheapest curr 1000000) ; shouldn't fail
              neighbor-tiles (valid-neighbors curr blockmap)
              neighbor-costs (calculate-neighbor-costs curr-cost neighbor-tiles blockmap)
              good-neighbors (filter (fn [[nbr cost]]
                                       (< cost (get cheapest nbr 1000000)))
                                     neighbor-costs)]
          (if (contains? goal-tiles curr)
            (build-path curr previous)
            (recur (apply conj
                          remaining 
                          (map (fn [[nbr cost]]
                                 [(+ cost (estimate-cost-to-target nbr goal-tiles)) nbr])
                               good-neighbors))
                   (apply conj
                          previous
                          (map (fn [[nbr _]]
                                 [nbr curr])
                               good-neighbors))
                   (apply conj cheapest good-neighbors))))))))
