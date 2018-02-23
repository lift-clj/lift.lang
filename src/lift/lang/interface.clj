(ns lift.lang.interface
  (:refer-clojure :exclude [type])
  (:require
   [clojure.core :as c]
   [lift.lang.signatures :as sig]
   [lift.lang.type :as t]
   [lift.lang.type.base :as base]
   [lift.lang.type.impl :as impl]
   [lift.lang.unification :as unify]
   [lift.lang.util :as u]
   [lift.lang.analyze :as ana]
   [lift.lang.rewrite :as rewrite]
   [lift.lang.inference :as infer]
   [lift.lang.type.def :as def]))

(base/import-syntax-types)
(base/import-type-types)
;; requirements of interfaces:
;; type syntax parser & ast
;; pattern matching for impl?
;; what is the type of a thing at runtime?
;; does it matter that the underlying system has a different type?

(def types
  {Long 'Integer})

(defn type [x]
  (let [t (c/type x)]
    (or (types t) t)))

(def interfaces (atom {}))

(defn uncurry [f]
  (fn [& args]
    (reduce #(% %2) f args)))

(defn get-impl [d vars f sig arglist]
  (let [sub (or (some->> arglist
                         (map (fn [s a]
                                (and (instance? Var s)
                                     (contains? vars (:a s))
                                     (t/sub {(:a s) (ana/type (Literal. a))})))
                              (sig/arrseq sig))
                         (filter identity)
                         (seq)
                         (apply unify/compose))
                t/id)
        d' (t/substitute d sub)]
    (uncurry
     (eval
      (rewrite/emit
       (rewrite/rewrite
        @t/type-env
        nil
        (get (get @t/type-env d') (u/resolve-sym f))))))))

(defn default-impl [f sig pred t impl]
  (let [{:keys [arglist args]} (sig/arglist sig)]
    `(defn ~f ~arglist
       (let [impl# (or (get-impl ~pred
                                 '~(set (rest t))
                                 '~f
                                 ~sig
                                 ~(vec (remove #{'&} arglist)))
                       ~impl
                       )]
         (apply impl# (apply concat ~args))))))

(defn type-sig-impl [f sig pred]
  `(swap! t/type-env assoc
          '~(u/resolve-sym f)
          (Forall. (t/ftv ~sig) (Predicated. [~pred] ~sig))))

(defn interface
  {:style/indent :defn}
  [t fn-list-defaults?]
  (let [[class & as] t
        pred (Predicate. class (mapv #(Var. %) as))
        fns (sig/parse-fn-list-default fn-list-defaults?)]
    `(do
       ~@(map (fn [[f _]] `(declare ~f)) fns)
       ~@(mapcat
          (fn [[f {:keys [sig impl]}]]
            `[~(default-impl f sig pred t impl)
              ~(type-sig-impl f sig pred)])
          fns)
       (swap! t/type-env assoc '~class ~pred)
       '~t)))

(def clojure-imports
  '#{Keyword Ratio Symbol})

(def literal-containers
  '#{List Vector})

(defn resolve-type-param [a]
  (let [r (resolve a)]
    (cond (contains? clojure-imports a)
          a
          (contains? literal-containers a)
          (symbol "lift.lang" (name a))
          (class? r)
          (symbol (.getSimpleName r))
          :else
          (def/resolve-sym a))))

(defn impl [[tag & as] impls]
  (let [as     (map resolve-type-param as)
        consts (mapv #(Const. %) as)
        tag-ts (map #(or (some-> (t/find-type @t/type-env %) infer/instantiate)
                         (Const. %)) as)
        [_ bs] (get @t/type-env tag)
        pred   (Predicate. tag consts)
        sub    (->> (map (fn [a [b]] [b a]) tag-ts bs) (into {}) t/sub)]
    `(do
       (swap! t/type-env assoc ~pred ~(sig/impl pred sub impls))
       '~pred)))
