(ns lift.lang.type.def
  (:require
   [clojure.spec.alpha :as s]
   [lift.lang.type.base :as base :refer [forall]]
   [lift.lang.util :as u]
   [lift.f.functor :as f]
   [lift.lang.inference :as infer]
   [lift.lang.unification :as unify]))

(base/import-container-types)
(base/import-type-types)

(s/def ::def*-decl
  (s/cat :doc? (s/? string?)
         :ann? (s/? ::type)
         :init any?))

(defn def* [[name & decl]]
  (let [{:keys [doc? ann? init]} (u/assert-conform ::def*-decl decl)
        name     (resolve-sym name)
        ann?     (when ann? (construct ann?))
        sigma1   (when ann? (forall (base/ftv ann?) ann?))
        _        (env/untern name)
        init'    (u/macroexpand-all init)
        _Gamma        (assoc @env/env name (Forall. #{} (Var. 'a)))
        [s1 syn] (infer/checks _Gamma init')
        [e1 t1]  (base/substitute syn s1)
        sigma2   (forall (base/ftv t1) t1)]
    (when ann?
      (unify/unify (infer/instantiate sigma1) (infer/instantiate sigma2)))
    `(do
       ~(if doc?
          `(def ~name ~doc? ~init)
          `(def ~name ~init))
       (infer/intern '~name ~(infer/prettify-vars (or sigma1 sigma2))))))
