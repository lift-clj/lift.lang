(ns check
  (:refer-clojure :exclude [case])
  (:require
   [clojure.set :refer [difference union]]
   [lift.lang.pattern :as p]
   [lift.lang.type :refer :all]
   [lift.lang.util :refer :all]
   [clojure.walk :as walk]
   [clojure.string :as string])
  (:import
   [lift.lang.type
    Apply Arrow Const Container Env Extend If Lambda Let Literal Record Restrict
    Row RowEmpty Scheme Select Substitution Symbol Unit Var]))

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
  (cond (= t (Var. a)) (sub {})
        (occurs? a t)  (throw (ex-info "Infinite Type" {:a a :t t}))
        :else          (sub {a t})))

(defn unify-compound [t1 tag1 t1-args t2 tag2 t2-args]
  (if (= tag1 tag2)
    (if (= (count t1-args) (count t2-args))
      (->> (map vector t1-args t2-args)
           (reduce (fn [s [t1 t2]] (compose (trampoline unify t1 t2) s))
                   (sub {})))
      (arity-error t1 t2))
    (unification-failure t1 t2)))

(p/defn rewrite-row
  ([l t [RowEmpty _]]
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

(p/defn unify
  ([[Arrow l r :as t1] [Arrow l' r' :as t2]]
   (let [s1 (unify l l')
         s2 (unify (substitute r s1) (substitute r' s1))]
     (compose s2 s1)))

  ([[Var a] t] (bind a t))

  ([t [Var a]] (bind a t))

  ([[Const a] [Const b] | (= a b)] (sub {}))

  ([[Container tag1 & t1-args :as t1] [Container tag2 & t2-args :as t2]]
   (unify-compound t1 tag1 t1-args t2 tag2 t2-args))

  ([[RowEmpty _] [RowEmpty _]] (sub {}))

  ([[Row k v tail :as row] [Row _ :as row']]
   ;; side ^^ condition needed here on tail before rewrite
   ;; must not already be bound (by bind)
   (let [[s row''] (rewrite-row k v row')]
     (compose (unify (substitute tail s) row'') s)))

  ([[Record row] [Record row']]
   (unify row row'))

  ([t1 t2]
   (unification-failure t1 t2)))

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
        z (int \z)
        α (int \α)]
    (zipmap (map (comp symbol str char) (cons α (range a (inc z))))
            (repeat (rest (range))))))

(let [vars (atom fresh-vars)]

  (defn refresh! []
    (reset! vars fresh-vars)
    nil)

  (defn fresh [& [a]]
    (if-let [[n] (get @vars a) ]
      (do
        (swap! vars update a rest)
        (Var. (symbol (str a n))))
      (do
        (swap! vars update 'α rest)
        (Var. (symbol (str \α (first (get @vars 'α)))))))))

(def fresh-vars
  (atom (for [a (map char (range 97 123)) i (rest (range))]
          (symbol (str a i)))))

(defn fresh [& _]
  (when-let [v (first @fresh-vars)]
    (swap! fresh-vars rest)
    (Var. v)))

(defn instantiate [{:keys [t vars]}]
  (let [nvars (map (fn [a] (fresh a)) vars)
        subst (sub (zipmap vars nvars))]
    (substitute t subst)))

(defn generalize [env t]
  (Scheme. t (difference (ftv t) (ftv env))))

(p/defn infer
  ([env [Literal a :as expr]]
   [(sub {}) (if (:type expr) expr (assoc expr :type (Const. a)))])

  ([env [Symbol a :as expr]]
   (if-let [s (or (when (= a '_?) (Scheme. (fresh) #{}))
                  (get env a)
                  (get env (resolve-sym a)))]
     [(sub {}) (merge (assoc expr :type (instantiate s)) (meta s))]
     (unbound-variable-error expr)))

  ([env [Lambda x e :as expr]]
   (let [tv      (fresh)
         env'    (assoc env (:a x) (Scheme. tv []))
         [s1 e'] (infer env' e)
         t1      (:type e')]
     [s1 (assoc expr
                :type (Arrow. (substitute tv s1) t1)
                :x (assoc x :type tv)
                :e e')]))

  ([env [Apply e1 e2 :as expr]]
   (let [tv       (fresh)
         [s1 e1'] (infer env e1)
         t1       (:type e1')
         [s2 e2'] (infer (substitute env s1) e2)
         t2       (:type e2')
         s3       (unify (substitute t1 s2) (Arrow. t2 tv))]
     [(compose s3 s2 s1)
      (assoc expr :type (substitute tv s3) :e1 e1' :e2 e2')]))

  ([env [Let x e1 e2 :as expr]]
   (let [[s1 e1'] (infer env e1)
         t1       (:type e1')
         env'     (substitute env s1)
         t        (generalize env' t1)
         [s2 e2'] (infer (assoc env' (:a x) t) e2)
         t2       (:type e2')]
     [(compose s2 s1) (assoc expr :type t2 :x (assoc x :type t) :e1 e1' :e2 e2')]))

  ([env [If cond then else :as expr]]
   (let [[s1 cond'] (infer env cond)
         t1         (:type cond')
         [s2 then'] (infer env then)
         t2         (:type then')
         [s3 else'] (infer env else)
         t3         (:type else')
         s4         (unify t1 (Const. 'Boolean))
         s5         (unify t2 t3)]
     [(compose s5 s4 s3 s2 s1)
      (assoc expr
             :type (substitute t2 s5)
             :cond cond'
             :then then'
             :else else')]))

  ([env [Select rec label :as expr]]
   (let [[s1 {t1 :type}]          (infer env label)
         valtype                  (fresh)
         basetype                 (fresh)
         seltype                  (Record. (Row. t1 valtype basetype))
         [s2 {t2 :type :as rec'}] (infer env rec)
         s3                       (unify seltype t2)]
     [(compose s3 s2) (assoc expr
                             :type (substitute valtype s3)
                             :rec (assoc rec :type t2))])))
