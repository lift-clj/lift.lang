(ns lift.lang.check
  (:refer-clojure :exclude [case])
  (:require
   [clojure.set :refer [difference union]]
   [lift.lang.pattern :as p]
   [lift.lang.type :refer :all]
   [lift.lang.util :refer :all]
   [clojure.walk :as walk]
   [clojure.string :as string]
   [clojure.core :as c]
   [lift.f.functor :as f])
  (:import
   [clojure.lang IPersistentVector IPersistentMap]
   [lift.lang.type
    Apply Arrow Const Container Env Extend If Lambda Let Literal Map Quantified
    Record Restrict Row RowEmpty Scheme Select Substitution Symbol Unit Var
    Vector]))

(def id (sub {}))

(defn occurs? [x expr]
  (contains? (ftv expr) x))

(declare unify)

(defn unify-merge [a b]
  (reduce-kv (fn [init k v]
               (if-let [v' (k init)]
                 (let [s (unify v' v)]
                   (-> init
                       (assoc k (substitute v' s))
                       (merge s)))
                 (assoc init k v)))
             a
             b))
;; Maybe this is the solution?

(defn compose
  ([s1 s2]
   (unify-merge s1 (fmap #(substitute % s1) s2)))
  ([s1 s2 & ss]
   (reduce compose (conj ss s2 s1))))

(defn bind [a t]
  (cond (= t (Var. a)) id
        (occurs? a t)  (throw (ex-info "Infinite Type" {:a a :t t}))
        :else          (sub {a t})))

(defn unify-compound [t1 tag1 t1-args t2 tag2 t2-args]
  (if (= tag1 tag2)
    (if (= (count t1-args) (count t2-args))
      (->> (map vector t1-args t2-args)
           (reduce (fn [s [t1 t2]] (compose (trampoline unify t1 t2) s))
                   id))
      (arity-error t1 t2))
    (unification-failure t1 t2)))

(p/defn rewrite-row
  ([l t [RowEmpty _]]
   (prn l t)
   (throw (Exception. (format "Row does not contain label %s" (show l)))))

  ([l t [Row l' t' tail] | (= l l')]
   [(trampoline unify t t') tail])

  ([l t [Row l' t' tail]]
   (let [[s tail'] (rewrite-row l t tail)]
     [s (Row. l' t' tail')]))

  ([l t [Var a]] ; this is the case where r is a tv
   (let [row (Row. l t (Var. a))]
     [(sub {a row}) row]))

  ([_ _ x]
   (throw (Exception. (format "Expected row type, got %s" (show x))))))

(defn unify-constraint [a b]
  (cond (= a b)    id
        (isa? a b) (sub {b a})
        (isa? b a) (sub {a b})
        :else      (unification-failure a b)))

(p/defn unify
  ([[Arrow l r :as t1] [Arrow l' r' :as t2]]
   (let [s1 (unify l l')
         s2 (unify (substitute r s1) (substitute r' s1))]
     (compose s2 s1)))

  ([[Var a] t] (bind a t))

  ([t [Var a]] (bind a t))

  ([[Const a] [Const b] | (= a b)] id)

  ([[Container tag1 & t1-args :as t1] [Container tag2 & t2-args :as t2]]
   (unify-compound t1 tag1 t1-args t2 tag2 t2-args))

  ([[RowEmpty _] [RowEmpty _]] id)

  ([[Row k v tail :as row] [Row _ :as row']]
   ;; side ^^ condition needed here on tail before rewrite
   ;; must not already be bound (by bind)
   (let [[s row''] (rewrite-row k v row')]
     (compose (unify (substitute tail s) row'') s)))

  ([[Record row] [Record row']]
   (unify row row'))

  ([[Quantified c t :as t1] [Quantified c' t' :as t2]]
   (let [s1 (unify-constraint c c')
         s2 (unify t t')]
     (compose s1 s2)))

  ([t1 t2]
   (unification-failure t1 t2)))

(defn unify-coll [coll]
  (reduce (fn [s [a b]] (compose (unify a b) s)) id (partition 2 1 coll)))

(defmacro unifies? [ast ts]
  `(try
     (unify (:type ~ast) (type-signature '~ts))
     true
     (catch clojure.lang.ExceptionInfo e#
       (if (-> e# ex-data :type (= :unification-failure))
         false
         (throw e#)))))

(def fresh-vars
  (let [a (int \a)
        z (int \z)]
    (zipmap (map (comp symbol str char) (range a (inc z)))
            (repeat (rest (range))))))

(def empty-env (env {:fresh fresh-vars :type {}}))

(defn fresh [env & [a]]
  (let [a (or a 'a)
        v (-> env :fresh (get a) first (->> (str a)) symbol Var.)]
    [(update-in env [:fresh a] rest) v]))

(defn instantiate [{:keys [t vars]}]
  (let [nvars (map (fn [a] (fresh a)) vars)
        subst (sub (zipmap vars nvars))]
    (substitute t subst)))

(defn generalize [env t]
  (Scheme. t (difference (ftv t) (ftv env))))

(defn lookup [{env :type} {a :a}]
  (or (when (= a '_) (Scheme. (fresh) #{})) ;; TODO: fresh no more
      (get env a)
      (get env (resolve-sym a))
      (unbound-variable-error a)))

(defn ret [env sub expr & [type]]
  [(assoc env :sub sub) (cond-> expr type (assoc :type type))])

(defprotocol Syntax
  (infer [expr env]))

(defn infer-coll [env type expr]
  (let [[env expr] (reduce (fn [[s xs] x]
                             (let [[s x] (x s)]
                               [s (conj xs x)]))
                           [env []]
                           expr)
        s (unify-coll (map :type expr))
        t (-> expr first :type (substitute s))]
    (ret env s {:expr expr} (Container. type [t]))))

(extend-protocol Syntax
  Literal
  (infer [expr env] (ret env id expr))

  Symbol
  (infer [expr env]
    (ret env id expr (instantiate (lookup env expr))))

  Lambda
  (infer [[x e] env]
    (let [[env tv] (fresh env (:a x))
          env (assoc-in env [:type (:a x)] (Scheme. tv []))
          [{s :sub :as env} e] (e env)
          t (Arrow. (substitute tv s) (:type e))]
      (ret env s (Lambda. (assoc x :type tv) e) t)))

  Apply
  (infer [[e1 e2] env]
    (let [[env tv] (fresh env)
          [{s1 :sub :as env} {t1 :type :as e1}] (e1 env)
          [{s2 :sub :as env} {t2 :type :as e2}] (e2 (substitute env s1))
          s3 (unify (substitute t1 s2) (Arrow. t2 tv))]
      (ret env (compose s3 s2 s1) (Apply. e1 e2) (substitute tv s3))))

  Let
  (infer [[x e1 e2] env]
    (let [[{s1 :sub :as env} {t1 :type :as e1}] (e1 env)
          env (substitute env s1)
          {t :type :as x} (assoc x :type (generalize env t1))
          env (assoc-in env [:type (:a x)] t)
          [{s2 :sub :as env} {t2 :type :as e2}] (e2 env)]
      (ret env (compose s2 s1) (Let. x e1 e2) t2)))

  If
  (infer [[cond then else] env]
    (let [[{s1 :sub :as env} {t1 :type :as cond}] (cond env)
          [{s2 :sub :as env} {t2 :type :as then}] (then env)
          [{s3 :sub :as env} {t3 :type :as else}] (else env)
          s4 (unify t1 (Const. 'lift/Boolean))
          s5 (unify t2 t3)
          sub (compose s5 s4 s3 s2 s1)]
      (ret env sub (If. cond then else) (substitute t2 s5))))

  Select
  (infer [[rec label] env]
    (let [[env {t1 :type}] (label env)
          [env vtype] (fresh env)
          [env btype] (fresh env)
          rectype (Record. (Row. t1 vtype btype))
          [{s2 :sub :as env} {t2 :type :as rec}] (rec env)
          s3 (unify rectype t2)]
      (ret env (compose s3 s2) (Select. rec label) (substitute vtype s3))))

  Vector
  (infer [expr env]
    (let [[env {:keys [expr type]}] (infer-coll env 'Vector (:xs expr))]
      (ret env (:sub env) (Vector. expr) type)))

  Map
  (infer [expr env]
    (let [[env {vals :expr}] (infer-coll env nil (vals expr))
          keys (keys expr)
          labels (map #(Const. %) keys)
          t (Record. (reduce (fn [row [k v]] (Row. k v row))
                                (RowEmpty.)
                                (map vector labels (map :type vals))))]
      (ret env (:sub env) (Map. (into {} (map vector keys vals))) t))))
