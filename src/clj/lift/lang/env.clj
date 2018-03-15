(ns lift.lang.env
  (:require [lift.lang.util :as u]))

(defonce env (ref {}))

(defn intern [name type]
  (dosync (alter env assoc name type))
  nil)

(defn untern [name]
  (dosync (alter env dissoc name))
  nil)

(defn resolve
  ([name]
   (resolve @env name))
  ([_Gamma name]
   (or (if-let [t (get _Gamma name)] [name t])
       (let [name' (u/ns-qualify name)]
         (if-let [t (get _Gamma name')] [name' t])))))
