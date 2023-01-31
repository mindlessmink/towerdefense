(ns towerdefense.tower)

(def ^:private tower-defs
  {:pellet {:cost 5}
   :squirt {:cost 15}
   :dart {:cost 20}})

(defrecord Tower [tower-type tower-def level x y id])

(defn make-tower [tower-type level x y]
  (if-let [tower-def (get tower-defs tower-type)]
    (Tower. tower-type
            tower-def
            level
            x
            y
            (gensym (str (name tower-type) "tower")))))

(defn tower-cost [tower-type]
  (get-in tower-defs [tower-type :cost]))
