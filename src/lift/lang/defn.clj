(ns lift.lang.defn
  (:refer-clojure :exclude [defn])
  (:require
   [clojure.core :as c]
   [clojure.spec.alpha :as s]
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

(s/def ::arg-pattern (s/alt :sym symbol? :tup ::args-pattern :dst ::case/dtor))
(s/def ::args-pattern (s/and vector? (s/+ ::arg-pattern)))
(s/def ::pattern (s/and seq? (s/cat :args ::args-pattern :expr any?)))

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
