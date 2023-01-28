(ns towerdefense.tower)

(defrecord Tower [tower-type level x y id])

(defn make-tower [tower-type level x y]
  (Tower. tower-type level x y (gensym (str (name tower-type) "tower"))))
