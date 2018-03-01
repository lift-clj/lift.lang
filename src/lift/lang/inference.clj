(ns lift.lang.inference
  (:require
   [lift.f.functor :as f]
   [lift.lang.pattern :as p]
   [lift.lang.unification :as unify :refer [compose unify]]
   [lift.lang.util :as u]
   [lift.lang.type.base :as base :refer [id $]]
   [lift.lang.type.impl :refer [cata hylo -vec]]
   [lift.lang.analyze :as ana]
   [clojure.set :as set]
   [lift.lang.type.impl :as impl])
  (:import
   [clojure.lang Fn]))

(def env (atom {}))

(base/import-container-types)
(base/import-type-types)
(base/import-syntax-types)

(impl/deftype (InferError e m)
  f/Functor
  (f/-map [x _] x))

(defn xi? [x]
  (and (symbol? x) (= \ξ (first (name x)))))

(p/defn assume?
  ([[Predicate _ as]]
   (letfn [(can? [a]
             (or (instance? Const a)
                 (and (instance? Var a) ;(xi? (:a a)) ;; TODO: this is probably wrong
                      )))]
     (every? can? as)))
  ([x] (throw (Exception. (format "Cannot assume %s" (pr-str x))))))

(defn lookup [_Gamma a]
  (or (and (= '_? a) [a (Forall. #{} (Var. (gensym 'a)))])
      (if-let [t (get _Gamma a)] [a t])
      (let [a' (u/resolve-sym a)] (if-let [t (get _Gamma a')] [a' t]))
      (u/unbound-variable-error a)))

(defn symhead [s]
  (if (symbol? s)
    (symbol (str (first (name s))))
    (throw (Exception. ("Cannot take symhead of non-symbol " s)))))

(defn instantiate [[as t :as x]]
  (let [vars  (mapv (comp #(Var. %) gensym symhead) as)
        subst (base/sub (zipmap as vars))]
    (base/substitute t subst)))

(defn generalize [env t]
  (Forall. (set/difference (base/ftv t) (base/ftv env)) t))

(defn release [t]
  (if (instance? Predicated t)
    (-vec t)
    [nil t]))

(p/defn concrete-instance?
  ([[Predicate _ as :as p]]
   (every? #(instance? Const %) as))
  ([_] false))

(p/defn concrete-instance?
  ([[Predicate _ as :as p]]
   (every? concrete-instance? as))
  ([[Container tag args]]
   true ;;(every? concrete-instance? args)
   )
  ([[Const _]] true)
  ([_] false))

(p/defn concrete-instance
  ([[Predicate tag as]]
   (Predicate. tag (mapv concrete-instance as)))
  ([[Container tag args]]
   (Const. (symbol (namespace tag) (name tag))))
  ([[Const _ :as c]] c)
  ([x] x))

(defn release? [_Gamma p]
  (let [p (if (concrete-instance? p)
            (concrete-instance p)
            p)]
    (or (nil? p)
        (contains? _Gamma p)
        (and (not (concrete-instance? p))
             (assume? p))
        (throw (Exception. (format "No instance of %s in Γ" (pr-str p)))))))

(defn with-pred [_Gamma preds t]
  (if (instance? Const t)
    t
    (let [shed (partial remove #(or (nil? %) (contains? _Gamma %)))]
      (or (some-> preds flatten shed distinct seq (Predicated. t))
          t))))

(p/defn split-pred
  ([[Predicate tag [Predicated [[Predicate _ as :as p]]]]]
   [(Predicate. tag as) p])
  ([p] [p]))

(defn hoist [t]
  (letfn [(ps [x] (when (instance? Predicated x) (:preds x)))
          (un [x] (if (instance? Predicated x) (:t x) x))]
    (let [ts (->> t -vec (mapcat ps) (remove nil?) distinct seq)
          t  (f/map un t)]
      (if ts (Predicated. ts t) t))))

(defn rel-unify [_Gamma a b]
  (let [[[pa ta] [pb tb]] (map release [a b])
        s  (unify ta tb)
        ps (->> (into pa pb)
                (remove nil?)
                (mapcat #(split-pred (base/substitute % s)))
                (distinct))]
    (every? (partial release? _Gamma) ps)
    [s ps]))

(defn infer-coll [_Gamma ctor tag coll]
  (let [[s1 coll']
        (reduce (fn [[s xs] x] (let [[s x] (x _Gamma)] [s (conj xs x)]))
                [id []]
                coll)
        s2 (unify/unify-coll (map :t coll'))
        t2 (if (seq coll')
             (-> coll' first :t (base/substitute s2))
             (Var. (gensym 'a)))]
    [s2 ($ (ctor coll') (Container. tag [t2]))]))

(defn syn-sym [x t]
  ($ (Symbol. x) t [] (assoc (meta x) :expr x)))

(p/defn -infer
  ([_Gamma [Literal _ :as expr]] [id ($ expr (ana/type expr))])

  ([_Gamma [Symbol a :as expr]]
   (let [[a t] (lookup _Gamma a)] [id ($ (Symbol. a) (instantiate t))]))

  ([_Gamma [Lambda [a] e]]
   (let [tv (Var. (gensym 'ξ))
         _Gamma (assoc _Gamma a (Forall. #{} tv))
         [s [_ t :as e]] (e _Gamma)
         [p1 t1] (release t)
         [p2 t2] (release (base/substitute tv s))]
     [s ($ (Lambda. (syn-sym a t2) e)
           (with-pred _Gamma [p1 p2] (Arrow. t2 t1)))]))

  ([_Gamma [Key k]] [id ($ (Key. k) (Const. k))])

  ([_Gamma [Apply e1 e2]]
   (let [tv (Var. (gensym 'ξ))
         [s1 [_ t1 :as e1]] (e1 _Gamma)
         [s2 [_ t2 :as e2]] (e2 (base/substitute _Gamma s1))
         [s3 ps] (rel-unify _Gamma (base/substitute t1 s2) (hoist (Arrow. t2 tv)))]
     [(compose s3 s2 s1)
      ($ (Apply. e1 e2) (with-pred _Gamma ps (base/substitute tv s3)))]))

  ([_Gamma [Let [x] e1 e2]]
   (let [[s1 [_ t1 :as e1]] (e1 _Gamma)
         _Gamma (base/substitute _Gamma s1)
         _Gamma (assoc _Gamma x (generalize _Gamma t1))
         [s2 [_ t2 :as e2]] (e2 _Gamma)]
      [(compose s2 s1) ($ (Let. (syn-sym x t1) e1 e2) t2)]))

  ([_Gamma [If cond then else]]
    (let [[s1 [_ t1 :as cond]] (cond _Gamma)
          [s2 [_ t2 :as then]] (then _Gamma)
          [s3 [_ t3 :as else]] (else _Gamma)
          [s4 p1] (rel-unify _Gamma t1 (Const. 'Boolean))
          [s5 p2] (rel-unify _Gamma t2 t3)]
      [(compose s5 s4 s3 s2 s1)
       ($ (If. cond then else)
          (with-pred _Gamma (concat p1 p2) (base/substitute t2 s5)))]))

  ([_Gamma [Select label rec]]
   (let [t1 (Const. label)
         vtype (Var. (gensym 'v))
         btype (Var. (gensym 'b))
         rectype (Record. (Row. t1 vtype btype))
         [s1 [_ t2 :as r]] (rec _Gamma)
         s2 (unify rectype t2)]
     [(compose s2 s1) ($ (Select. label r) (base/substitute vtype s2))]))

  ([_Gamma [Restrict label rec]]
   (let [t1 (Const. label)
         vtype (Var. (gensym 'v))
         btype (Var. (gensym 'b))
         rectype (Record. (Row. t1 vtype btype))
         [s1 [_ t2 :as r]] (rec _Gamma)
         s2 (unify rectype t2)]
     [(compose s2 s1)
      ($ (Restrict. label r) (Record. (base/substitute btype s2)))]))

  ([_Gamma [List xs]]
   (infer-coll _Gamma #(List. %) 'lift.lang/List xs))

  ([_Gamma [Vector xs]]
   (infer-coll _Gamma #(Vector. %) 'lift.lang/Vector xs))

  ([_Gamma [Map m]]
   (let [[s1 vals] (reduce (fn [[s2 es] e]
                             (let [[s3 e'] (e _Gamma)]
                               [(compose s3 s2) (conj es e')]))
                           [id []]
                           (vals m))
          keys (keys m)
          labels (map #(Const. %) keys)
          t (Record. (reduce (fn [row [k v]] (Row. k v row))
                                (RowEmpty.)
                                (map vector labels (map :t vals))))]
      [s1 ($ (Map. (into {} (map vector keys vals))) t)]))

  ([_Gamma [Prim f t]] [id (Prim. f (instantiate t))])

  ([_Gamma [SyntaxNode n t e m]]
   (try
     (let [[s1 [e1 t1]] (n _Gamma)
           errs (->> (impl/-vec e1)
                     (filter #(instance? SyntaxNode %))
                     (mapcat :e)
                     (filterv identity))]
       [s1 (base/$ e1 t1 errs m)])
     (catch Throwable t
       (let [err (InferError. (.getMessage t) m)]
         [id (base/$ err (Var. 'a) [err] m)]))))

  ([_Gamma [Mark a]] (a _Gamma))

  ([_ x]
   (throw (Exception. (str "Unrecognized syntax: " (pr-str x))))))

(defn infer [_Gamma expr]
  (letfn [(infer-f [x] (fn [env] (-infer env x)))]
    ((cata infer-f expr) _Gamma)))

(defn -infer-ann-err [x]
  (fn [env]
    (try
      (-infer env x)
      (catch Throwable t
        ;; [id (base/$ x (Var. 'err) [(str "Inference failure: " (.getMessage t))])]
        (throw t)))))

(defn check
  ([expr] (check @env expr))
  ([env expr]
   (let [[s ast] ((hylo -infer-ann-err ana/parse expr) env)]
     (base/substitute ast s))))

(defn checks
  ([expr] (checks @env expr))
  ([env expr]
   ((hylo -infer-ann-err ana/parse expr) env)))

(defn pretty-sub
  ([[f & ftvs] [v & vars]]
   (if f (assoc (pretty-sub ftvs vars) f v) {}))
  ([ftvs]
   (-> (sort ftvs)
       (pretty-sub (map (comp #(Var. %) symbol str char) (range 97 123)))
       (base/sub))))

(p/defn prettify-vars
  ([[Forall as t]]
   (let [[m :as sub] (pretty-sub (base/ftv t))]
     (Forall. (set (map (comp :a m) as)) (base/substitute t sub))))
  ([t]
   (base/substitute t (pretty-sub (base/ftv t)))))
