(ns lift.lang.unification
  (:require
   [clojure.set :refer [difference union]]
   [lift.f.functor :as f]
   [lift.lang.pattern :as p]
   [lift.lang.type :as t]
   [lift.lang.util :as u])
  (:import
   [lift.lang.type
    Apply Arrow Const Forall Lambda Literal Predicate Predicated Symbol Var
    Env
    ]))

(def id (t/sub {}))

(defn xi? [x]
  (and (symbol? x) (= \ξ(first (name x)))))

(defn occurs? [x expr]
  ;; (prn 'occurs? x expr)
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
  ;; (prn 'arr l l' r r')
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

;; (extend-protocol t/Substitutable
;;   clojure.lang.IPersistentMap
;;   (t/substitute [x s]
;;     (f/map #(t/substitute % s) x))
;;   clojure.lang.Keyword
;;   (t/substitute [x _] x))

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

(t/substitute (Env. _Gamma) (t/sub {'a (Var. 'z)}))

(f/map (constantly 1) (Predicated. [(Predicate. 'Eq (Const. 'Int))] 3))

(t/env {'x (Var. 'y)})

(defn lookup [_Gamma [a]]
  (prn _Gamma a)
  (or (get _Gamma a)
      (get _Gamma (u/resolve-sym a))
      (u/unbound-variable-error a)))

(defn instantiate [{:keys [t vars] :as x}]
  ;; (prn x)
  (let [nvars (map (comp #(Var. %) gensym) vars)
        subst (t/sub (zipmap vars nvars))]
    ;; (prn subst)
    ;; (prn (t/substitute t subst))
    (t/substitute t subst)))

(defn release [t]
  (if (instance? Predicated t)
    [(.-pred t) (.-t t)]
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

(defn rel-unify [_Gamma a b]
  ;; (prn a \, b)
  (let [[[pa ta] [pb tb]] (map release [a b])
        s  (unify ta tb)
        ps (distinct (map #(t/substitute % s) (remove nil? (into pa pb))))]
    ;; (prn ps)
    (every? (partial release? _Gamma) ps)
    [s ps]))

(defn cata [f x]
  (f (f/map #(cata f %) x)))

(defn hoist [t]
  (letfn [(ps [x] (when (instance? Predicated x) (.-pred x)))
          (un [x] (if (instance? Predicated x) (.-t x) x))]
    (or (some-> (ps t) (Predicated. (un t))) t)))

(p/defn infer
  ([_Gamma [Literal _ :as expr]] [id (:type expr)])

  ([_Gamma [Symbol _ :as expr]] [id (instantiate (lookup _Gamma expr))])

  ([_Gamma [Lambda [a] e]]
   (let [tv      (Var. (gensym 'ξ))
         _Gamma       (assoc _Gamma a (Forall. #{} tv))
         [s t]   (infer _Gamma e)
         [p1 t1] (release t)
         [p2 t2] (release (t/substitute tv s))]
     ;; (prn p1 p2)
     [s (with-pred _Gamma [p1 p2] (Arrow. t2 t1))]))

  ([_Gamma [Apply e1 e2]]
    (let [tv      (Var. (gensym 'ξ))
          ;; _ (prn e1)
          [s1 t1] (infer _Gamma e1)
          ;; _ (prn t1)
          [s2 t2] (infer (t/substitute _Gamma s1) e2)
          [s3 ps] (rel-unify _Gamma (t/substitute t1 s2) (hoist (Arrow. t2 tv)))]
      [(compose s3 s2 s1) (with-pred _Gamma ps (t/substitute tv s3))])))

(defn lit [v t]
  (-> v Literal. (assoc :type (Const. t))))

(->>
 (Lambda. (Symbol. 'a)
          (Lambda. (Symbol. 'b)
                   (Lambda. (Symbol. 'c)
                            (-> (Symbol. 'eq)
                                (Apply. (-> (Symbol. '+)
                                            (Apply. (Symbol. 'b))
                                            (Apply. (Symbol. 'c))))
                                (Apply. (Symbol. 'a))))))
 (infer _Gamma)
 (second))


;; (def eq (instantiate (_Gamma 'eq)))

;; (unify (assoc _Gamma (Var. 'a1) ::assumed)
;;        eq
;;        (Arrow. (Var. 'a1) (Arrow. (Var. 'a1) (Var. 'b1))))
