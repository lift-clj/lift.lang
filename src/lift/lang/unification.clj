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
  (if (= tag1 tag2)
    (if (= (count t1-args) (count t2-args))
      (->> (map vector t1-args t2-args)
           (reduce (fn [s [t1 t2]] (compose (trampoline unify t1 t2) s))
                   id))
      (u/arity-error t1 t2))
    (u/unification-failure t1 t2)))

(p/defn unify
  ([[Arrow l r :as t1] [Arrow l' r' :as t2]] (unify-arrow l l' r r'))

  ([[Var a] t] (bind a t))
  ([t [Var a]] (bind a t))

  ([[Const a] [Const b] | (= a b)] id)

  ([[Container tag1 t1-args :as t1] [Container tag2 t2-args :as t2]]
   (unify-compound t1 tag1 t1-args t2 tag2 t2-args))

  ([t1 t2] (u/unification-failure t1 t2)))