(ns lift.lang.defn
  (:require
   [lift.lang.case :as case]))

(defn vars [n]
  (take n (map (comp symbol str char) (range 97 123))))

(defn defn* [name exprs]
  (let [n  (count (ffirst exprs))
        vs (vars n)]
    `(defn ~name
       {:arglists '~(map first exprs)}
       [~@vs]
       ~(case/case*
         (case/tuple n vs)
         (mapcat (fn [[match expr]] `[~(case/tuple n match) ~expr])
                 exprs)))))

(defmacro defn [name & decl]
  (defn* name decl))
