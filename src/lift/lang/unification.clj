(ns lift.lang.unification
  (:require
   [clojure.set :refer [difference union]]
   [lift.f.functor :as f]
   [lift.lang.analyze :as ana]
   [lift.lang.pattern :as p]
   [lift.lang.type :as t]
   [lift.lang.type.base :as base]
   [lift.lang.type.impl :refer [cata]]
   [lift.lang.util :as u]))

(base/import-container-types)
(base/import-type-types)

(def id (t/sub {}))

(defn occurs? [x expr]
  (contains? (t/ftv expr) x))

(declare unify)

(defn unify-merge [a b]
  (reduce-kv (fn [init k v]
               (if-let [v' (k init)]
                 (let [[s] (unify v' v)]
                   (-> init
                       (assoc k (t/substitute v' s))
                       (merge s)))
                 (assoc init k v)))
             a
             b))

(defn compose
  ([s1 s2]
   (unify-merge s1 (f/map #(t/substitute % s1) s2)))
  ([s1 s2 & ss]
   (reduce compose (conj ss s2 s1))))

(defn bind [a t]
  (cond (= t (Var. a)) id
        (occurs? a t)  (throw (ex-info "Infinite Type" {:a a :t t}))
        :else          (t/sub {a t})))

(defn unify-arrow [l l' r r']
  (let [s1 (unify l l')
        s2 (unify (t/substitute r s1) (t/substitute r' s1))]
    (compose s2 s1)))

(defn unify-compound [t1 tag1 t1-args t2 tag2 t2-args]
  (letfn [(unify-args  [s1]
            (->> (map vector t1-args t2-args)
                 (reduce (fn [s [t1 t2]] (compose (trampoline unify t1 t2) s))
                         s1)))]
    (if (= (count t1-args) (count t2-args))
      (do
        (prn 'ttt (type tag1) (type tag2))
        (if (= tag1 tag2)
         (unify-args id)
         (cond (and (instance? Var tag1) (instance? Var tag2))
               (unify-args (unify tag1 tag2))
               (instance? Var tag1)
               (unify-args (unify tag1 (Const. tag2)))
               (instance? Var tag2)
               (unify-args (unify (Const. tag1) tag2))
               :else
               (u/unification-failure t1 t2))))
      (u/arity-error t1 t2))))

(defn unify-coll [coll]
  (reduce (fn [s [a b]] (compose (unify a b) s)) id (partition 2 1 coll)))

(p/defn rewrite-row
  ([l t [RowEmpty _]]
   (throw (Exception. (format "Row does not contain label %s" (pr-str l)))))

  ([l t [Row l' t' tail] | (= l l')]
   [(trampoline unify t t') tail])

  ([l t [Row l' t' tail]]
   (let [[s tail'] (rewrite-row l t tail)]
     [s (Row. l' t' tail')]))

  ([l t [Var a]] ; this is the case where r is a tv
   (let [row (Row. l t (Var. a))]
     [(t/sub {a row}) row]))

  ([_ _ x]
   (throw (Exception. (format "Expected row type, got %s" (pr-str x))))))

(p/defn unify
  ([[Arrow l r :as t1] [Arrow l' r' :as t2]] (unify-arrow l l' r r'))

  ([[Var a] t] (bind a t))
  ([t [Var a]] (bind a t))

  ([[Const a] [Const b] | (= a b)] id)

  ([[Container tag1 t1-args :as t1] [Container tag2 t2-args :as t2]]
   (unify-compound t1 tag1 t1-args t2 tag2 t2-args))

  ([[RowEmpty _] [RowEmpty _]] id)

  ([[Row k v tail :as row] [Row _ :as row']]
   ;; side ^^ condition needed here on tail before rewrite
   ;; must not already be bound (by bind)
   (let [[s row''] (rewrite-row k v row')]
     (compose (unify (t/substitute tail s) row'') s)))

  ([[Record row] [Record row']]
   (unify row row'))

  ([t1 t2] (u/unification-failure t1 t2)))
