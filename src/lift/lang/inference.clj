(ns lift.lang.inference
  (:require
   [lift.f.functor :as f]
   [lift.lang.pattern :as p]
   [lift.lang.unification :refer [compose unify]]
   [lift.lang.util :as u]
   [lift.lang.type :as t :refer [id import-syntax-types import-type-types]]
   [lift.lang.type.impl :refer [cata hylo -vec]]
   [lift.lang.analyze :as ana]))

(import-type-types)
(import-syntax-types)

(defn xi? [x]
  (and (symbol? x) (= \ξ(first (name x)))))

(p/defn assume?
  ([[Predicate _ as]]
   (letfn [(can? [a]
             (or (instance? Const a)
                 (and (instance? Var a) (xi? (:a a)))))]
     (every? can? as)))
  ([x] (throw (Exception. (format "Cannot assume %s" (pr-str x))))))

(defn lookup [_Gamma a]
  (or (get _Gamma a)
      (get _Gamma (u/resolve-sym a))
      (u/unbound-variable-error a)))

(defn instantiate [[as t :as x]]
  (let [vars  (map (comp #(Var. %) gensym) as)
        subst (t/sub (zipmap as vars))]
    (t/substitute t subst)))

(defn release [t]
  (if (instance? Predicated t)
    (-vec t)
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
  ([[Predicate tag [Predicated [[Predicate _ as :as p]]]]]
   [(Predicate. tag as) p])
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
    (let [ts (->> t -vec (mapcat ps) (remove nil?) distinct seq)
          t  (f/map un t)]
      (if ts (Predicated. ts t) t))))

(defn $ [expr type]
  (SyntaxNode. expr type))

(p/defn -infer
  ([_Gamma [Literal _ :as expr]] [id ($ expr (ana/type expr))])

  ([_Gamma [Symbol a :as expr]] [id ($ expr (instantiate (lookup _Gamma a)))])

  ([_Gamma [Lambda [a] e]]
   (let [tv (Var. (gensym 'ξ))
         _Gamma (assoc _Gamma a (Forall. #{} tv))
         [s [_ t :as e]] (e _Gamma)
         [p1 t1] (release t)
         [p2 t2] (release (t/substitute tv s))]
     [s ($ (Lambda. (Symbol. a) e) (with-pred _Gamma [p1 p2] (Arrow. t2 t1)))]))

  ([_Gamma [Apply e1 e2]]
   (let [tv (Var. (gensym 'ξ))
         [s1 [_ t1 :as e1]] (e1 _Gamma)
         [s2 [_ t2 :as e2]] (e2 (t/substitute _Gamma s1))
         [s3 ps] (rel-unify _Gamma (t/substitute t1 s2) (hoist (Arrow. t2 tv)))]
     [(compose s3 s2 s1)
      ($ (Apply. e1 e2) (with-pred _Gamma ps (t/substitute tv s3)))])))

(defn infer [_Gamma expr]
  (letfn [(infer-f [x] (fn [env] (-infer env x)))]
    ((cata infer-f expr) _Gamma)))

(defn check
  ([expr] (check @t/type-env expr))
  ([env expr]
   (let [[s ast] ((hylo (fn [x] (fn [env] (-infer env x))) ana/parse expr) env)]
     (t/substitute ast s))))
