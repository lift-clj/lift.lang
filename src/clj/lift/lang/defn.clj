(ns lift.lang.defn
  (:refer-clojure :exclude [defn])
  (:require
   [clojure.core :as c]
   [clojure.spec.alpha :as s]
   [lift.lang.env :as env]
   [lift.lang.case :as case]))

(c/defn vars [n]
  (take n (map (comp symbol str char) (range 97 123))))

(c/defn defn-pattern [name exprs]
  (let [n  (count (ffirst exprs))
        vs (vars n)]
    `(c/defn ~name
       {:arglists '~(map first exprs)}
       [~@vs]
       ~(case/case*
         (case/tuple n vs)
         (mapcat (fn [[match expr]] `[~(case/tuple n match) ~expr])
                 exprs)))))

(c/defn fn-pattern [exprs]
  (let [n  (count (ffirst exprs))
        vs (vars n)]
    `(c/fn [~@vs]
       ~(case/case*
         (case/tuple n vs)
         (mapcat (fn [[match expr]] `[~(case/tuple n match) ~expr])
                 exprs)))))

(s/def ::arglist      (s/coll-of symbol? :kind vector?))
(s/def ::no-pattern   (s/cat :arglist ::arglist :expr any?))
(s/def ::arg-pattern  (s/alt :sym symbol? :tup ::args-pattern :dst ::case/dtor))
(s/def ::args-pattern (s/and vector? (s/+ ::arg-pattern)))
(s/def ::one-pattern  (s/cat :arglist ::args-pattern :expr any?))
(s/def ::pattern      (s/and seq? ::one-pattern))

(s/conform ::pattern
           '([l t (Row l' t' tail)]
             | (= l l')
             [(trampoline unify t t') tail])
           )

(c/defn pattern-match? [decl]
  (s/valid? (s/coll-of ::pattern) decl))

(c/defn defn-std [name decl]
  `(c/defn ~name ~@decl))

(c/defn defn* [name decl]
  (if (pattern-match? decl)
    (defn-pattern name decl)
    (defn-std name decl)))

(defmacro defn [name & decl]
  (defn* name decl))

(defn fn- [decl]
  (cond (pattern-match? decl)
        (fn-pattern decl)
        (s/valid? ::no-pattern decl)
        `(c/fn ~@decl)
        (s/valid? ::one-pattern decl)
        (fn-pattern (list decl))
        :else
        `(c/fn ~@decl)))

(defmacro special [name & decl]
  `(do
     (defmacro ~name ~@decl)
     (let [v# (resolve '~name)] (swap! env/specials conj v#) v#)))
