(ns lift.lang.namespace
  (:refer-clojure :exclude [find-ns intern ns-resolve])
  (:require
   [clojure.core.specs.alpha :as cs]
   [clojure.spec.alpha :as s]
   [lift.lang.util :as u]
   [clojure.core :as c]
   [clojure.set :as set]))

(def namespaces (ref {}))

(defn intern [ns name val]
  (dosync (alter namespaces assoc-in [ns :mappings name] val)))

(defn untern [ns name val]
  (dosync (alter namespaces update-in [ns :mappings] dissoc name)))

(defn find-ns [sym]
  (-> @namespaces (get sym)))

(defn ns-meta [ns]
  (-> ns find-ns :meta))

(defn dialect [ns]
  (-> ns ns-meta :lang))

(defn ns-resolve [ns sym]
  (if-let [nspace (-> sym namespace symbol)]
    (recur (or (-> ns find-ns :aliases nspace) nspace) (-> sym name symbol))
    (-> ns find-ns :mappings sym :var)))

(s/def ::ns-form
  (s/cat :_ (s/? #{'ns})
         :name simple-symbol?
         :docstring (s/? string?)
         :attr-map (s/? map?)
         :clauses (s/* ::cs/ns-require)))

(defn analyze-dialect [ns-form]
  (->> ns-form (u/assert-conform ::ns-form) :attr-map :lang))

(defmulti prelude :lang)

(defmethod prelude :lift/clojure [_] 'lift.lang)

(defn ns* [ns-form]
  (let [{:keys [name docstring attr-map]} (u/assert-conform ::ns-form ns-form)
        prelude (prelude attr-map)
        imports (some-> prelude ns-publics keys)
        mapped  (set/intersection (set imports)
                                  (set (some-> name c/find-ns ns-map keys)))]
    (prn imports)
    `(do
       (in-ns '~name)
       (.resetMeta (clojure.lang.Namespace/find '~name)
                   ~(cond-> attr-map docstring (assoc :doc docstring)))
       ~(if prelude
          `(do
             (doseq [m# (keys (ns-map '~name))] (ns-unmap '~name m#))
             (with-loading-context (refer '~prelude))
             (with-loading-context
              (refer 'clojure.core :exclude '~imports)))
          `(with-loading-context (refer 'clojure.core)))
       nil)))

(defn in-ns* [name]
  `(in-ns '~name))
