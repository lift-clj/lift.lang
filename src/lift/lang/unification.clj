(ns lift.lang.unification
  (:require
   [clojure.set :refer [difference union]]
   [lift.f.functor :as f]
   [lift.lang.analyze :as ana]
   [lift.lang.pattern :as p]
   [lift.lang.type :as t]
   [lift.lang.type.impl :refer [cata]]
   [lift.lang.util :as u])
  (:import
   [lift.lang.type
    Apply Arrow Const Forall Lambda Literal Predicate Predicated Symbol Var
    Env SyntaxNode]))

(def id (t/sub {}))

(defn xi? [x]
  (and (symbol? x) (= \ξ(first (name x)))))

(defn occurs? [x expr]
  (contains? (t/ftv expr) x))

(declare unify)

(defn unify-merge [a b]
  (reduce-kv (fn [init k v]
               (if-let [v' (k init)]
                 (let [s (unify v' v)]
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

(defn assume? [p]
  (xi? (.. p -a -a)))

(p/defn unify
  ([[Arrow l r :as t1] [Arrow l' r' :as t2]] (unify-arrow l l' r r'))

  ([[Var a] t] (bind a t))
  ([t [Var a]] (bind a t))

  ([[Const a] [Const b] | (= a b)] id)

  ([t1 t2] (u/unification-failure t1 t2)))

(def _Gamma
  {'eq (Forall. #{'a}
                (Predicated. (list (Predicate. 'Eq (Var. 'a)))
                             (Arrow. (Var. 'a)
                                     (Arrow. (Var. 'a)
                                             (Const. 'Bool)))))
   '+ (Forall. #{'a}
               (Predicated. (list (Predicate. 'Num (Var. 'a)))
                            (Arrow. (Var. 'a)
                                    (Arrow. (Var. 'a) (Var. 'a)))))
   (Predicate. 'Eq (Const. 'Int)) ::static
   (Predicate. 'Num (Const. 'Int)) ::static
   })


(defn lookup [_Gamma a]
  (or (get _Gamma a)
      (get _Gamma (u/resolve-sym a))
      (u/unbound-variable-error a)))

(defn instantiate [[as t]]
  (let [vars  (map (comp #(Var. %) gensym) as)
        subst (t/sub (zipmap as vars))]
    (t/substitute t subst)))

(defn release [t]
  (if (instance? Predicated t)
    (vec t)
    [nil t]))

(defn release? [_Gamma p]
  (or (nil? p)
      (contains? _Gamma p)
      (assume? p)
      (throw (Exception. (format "No instance of %s in Γ" (pr-str p))))))

(defn with-pred [_Gamma preds t]
  (let [shed (partial remove #(or (nil? %) (contains? _Gamma %)))]
    (or (some-> preds flatten shed distinct seq (Predicated. t))
        t)))

(p/defn split-pred
  ([[Predicate tag [Predicated [[Predicate _ a :as p]]]]]
   [(Predicate. tag a) p])
  ([p] [p]))

(defn rel-unify [_Gamma a b]
  (let [[[pa ta] [pb tb]] (map release [a b])
        s  (unify ta tb)
        ps (->> (into pa pb)
                (remove nil?)
                (mapcat #(split-pred (t/substitute % s)))
                (distinct))]
    (every? (partial release? _Gamma) ps)
    [s ps]))

(defn hoist [t]
  (letfn [(ps [x] (when (instance? Predicated x) (:preds x)))
          (un [x] (if (instance? Predicated x) (:t x) x))]
    (let [ts (->> t (mapcat ps) (remove nil?) distinct seq)
          t  (f/map un t)]
      (if ts (Predicated. ts t) t))))

(p/defn -infer
  ([_Gamma [Literal _ :as expr]]
   [id (ana/type expr)])

  ([_Gamma [Symbol a :as expr]] [id (instantiate (lookup _Gamma a))])

  ([_Gamma [Lambda [a] e]]
   (let [tv      (Var. (gensym 'ξ))
         _Gamma       (assoc _Gamma a (Forall. #{} tv))
         [s t]   (e _Gamma)
         [p1 t1] (release t)
         [p2 t2] (release (t/substitute tv s))]
     [s (with-pred _Gamma [p1 p2] (Arrow. t2 t1))]))

  ([_Gamma [Apply e1 e2]]
    (let [tv      (Var. (gensym 'ξ))
          [s1 t1] (e1 _Gamma)
          [s2 t2] (e2 (t/substitute _Gamma s1))
          [s3 ps] (rel-unify _Gamma (t/substitute t1 s2) (hoist (Arrow. t2 tv)))]
      [(compose s3 s2 s1) (with-pred _Gamma ps (t/substitute tv s3))])))

(defn infer [_Gamma expr]
  ((cata (fn [x]
           (fn [env] (-infer env x))
           ;;(update (-infer _Gamma expr) 1 #(SyntaxNode. expr %))
           )
         expr)
   _Gamma))

(->>
 (Lambda. (Symbol. 'a)
          (Lambda. (Symbol. 'b)
                   (Lambda. (Symbol. 'c)
                            (-> (Symbol. 'eq)
                                (Apply. (-> (Symbol. '+)
                                            (Apply. (Symbol. 'b))
                                            (Apply. (Symbol. 'c))))
                                (Apply. (Literal. 5))))))
 ;; (t/syntax)
 (infer _Gamma)
 (second)
 )
