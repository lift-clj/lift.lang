(ns lift.lang.type
  (:refer-clojure :exclude [case def])
  (:require
   [clojure.core :as c]
   [clojure.core.protocols :refer [IKVReduce]]
   [clojure.java.io :as io]
   [clojure.set :refer [difference union]]
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
   [lift.f.functor :as f :refer [Functor]]
   [lift.lang.util :as u]
   [lift.lang.type.base :as base]
   [lift.lang.type.def :as def]))

(base/import-container-types)
(base/import-infer-types)
(base/import-type-types)

(defn sub [s]
  (Substitution. s))

(def id (sub {}))

(defn env [e]
  (Env. e))

(def type-env def/type-env)
(def ftv base/ftv)
(def substitute base/substitute)

(defn find-type [_Gamma tag]
  (some (fn [[k v]] (and (instance? Container k) (= tag (:tag k)) v)) _Gamma))

(defn get-type [_Gamma tag]
  (get _Gamma tag))

(defmacro t [x]
  `(get @type-env '~(u/resolve-sym x)))

(defn ex-unknown-type [t]
  (throw (ex-info (format "Unknown Type %s" (pr-str t))
                  {:type ::unknown-type :t t})))

(defn unmatched-case-error [x]
  (throw (ex-info "Unmatched Case" {:type :unmatched-case-error :x x})))

;; (prim Symbol)
;; (lift.lang.type/def t/unmatched-case-error a)
;; (lift.lang.type/def def (Symbol -> a -> ()))
;; (lift.lang.type/def restrict (l -> {l a | r} ->  {| r}))

(defmacro def [name sig]
  `(def/intern-signature ~name ~sig))
