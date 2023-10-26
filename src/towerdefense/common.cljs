(ns towerdefense.common)

(defn game-over? [state]
  (<= (:lives state) 0))
