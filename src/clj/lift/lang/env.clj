(ns lift.lang.env
  (:require
   [lift.lang.type.base :as base]
   [lift.lang.util :as u])
  (:import
   [lift.lang.type.base Forall]))

(defonce env (ref {}))
(defonce specials (atom #{}))

(defn intern [name type]
  ;; let sigma (if (instance? Forall type) type (Forall. (base/ftv type) type))
  (dosync (alter env assoc name type))
  ;;sigma
  )

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
