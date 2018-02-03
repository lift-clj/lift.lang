(ns lift.lang.unification
  (:require
   [clojure.set :refer [difference union]]
   [lift.f.functor :as f]
   [lift.lang.pattern :as p]
   [lift.lang.type :as t]
   [lift.lang.util :as u])
  (:import
   [lift.lang.type
    Arrow Const Predicate Predicated Scheme Var
    Literal Symbol Lambda Apply]))

(def id (t/sub {}))

(defn xi? [x]
  (and (symbol? x) (= \ξ(first (name x)))))

(defn occurs? [x expr]
  ;; (prn 'occurs? x expr)
  (contains? (t/ftv expr) x))

(declare unify)

(defn unify-merge [_Gamma a b]
  (reduce-kv (fn [init k v]
               (if-let [v' (k init)]
                 (let [s (unify _Gamma v' v)]
                   (-> init
                       (assoc k (t/substitute v' s))
                       (merge s)))
                 (assoc init k v)))
             a
             b))

(defn compose
  ([_Gamma s1 s2]
   (merge s1 (f/map #(t/substitute % s1) s2)))
  ([_Gamma s1 s2 & ss]
   (reduce (partial compose _Gamma) (conj ss s2 s1))))

(defn bind [a t]
  (cond (= t (Var. a)) id
        (occurs? a t)  (throw (ex-info "Infinite Type" {:a a :t t}))
        :else          (t/sub {a t})))

(defn unify-arrow [_Gamma l l' r r']
  ;; (prn 'arr l l' r r')
  (let [s1 (unify _Gamma l l')
        s2 (unify _Gamma (t/substitute r s1) (t/substitute r' s1))]
    (compose _Gamma s2 s1)))

(defn assume? [p]
  (xi? (.. p -a -a)))

(p/defn unify
  ([_Gamma [Arrow l r :as t1] [Arrow l' r' :as t2]] (unify-arrow _Gamma l l' r r'))

  ([_Gamma t1 [Predicated _ :as t2]] (unify _Gamma t2 t1))
  ([_Gamma [Predicated p t :as t1] t2]
   ;; (prn 'unify t t2)
   (let [s1 (unify _Gamma t t2)
         ps (t/substitute p s1)]
     ;; (prn 'ps ps)
     (reduce (fn [s p]
               (cond (contains? _Gamma p) s
                     (assume? p)
                     (let [a (.-a p)
                           pred (t/sub {(.-a a) (Predicated. (list p) a)})]
                       (merge s (t/substitute pred s)))
                     :else
                     (throw (Exception. (format "No instance of %s in Γ" (pr-str p))))))
             s1
             ps)))

  ([_Gamma [Var a] t] (bind a t))
  ([_Gamma t [Var a]] (bind a t))

  ([_Gamma [Const a] [Const b] | (= a b)] id)

  ([_Gamma t1 t2] (u/unification-failure t1 t2)))

(extend-protocol t/Substitutable
  clojure.lang.IPersistentMap
  (t/substitute [x s]
    (f/map #(t/substitute % s) x))
  clojure.lang.Keyword
  (t/substitute [x _] x))

(def _Gamma
  {'eq (Scheme. (Predicated. (list (Predicate. 'Eq (Var. 'a)))
                             (Arrow. (Var. 'a)
                                     (Arrow. (Var. 'a)
                                             (Const. 'Bool))))
                #{'a})
   '+ (Scheme. (Predicated. (list (Predicate. 'Num (Var. 'a)))
                            (Arrow. (Var. 'a)
                                    (Arrow. (Var. 'a) (Var. 'a))))
               #{'a})
   (Predicate. 'Eq (Const. 'Int)) ::static
   (Predicate. 'Num (Const. 'Int)) ::static
   })

(defn lookup [_Gamma {a :a}]
  (or (get _Gamma a)
      (get _Gamma (u/resolve-sym a))
      (u/unbound-variable-error a)))

(defn instantiate [{:keys [t vars]}]
  (let [nvars (map (comp t/->Var gensym) vars)
        subst (t/sub (zipmap vars nvars))]
    (t/substitute t subst)))

(defn release [t]
  (if (instance? Predicated t)
    [(.-pred t) (.-t t)]
    [nil t]))

(p/defn infer
  ([_Gamma [Literal _ :as expr]] [id (:type expr)])

  ([_Gamma [Symbol _ :as expr]] [id (instantiate (lookup _Gamma expr))])

  ([_Gamma [Lambda [a] e]]
   (let [tv      (Var. (gensym 'ξ))
         _Gamma       (assoc _Gamma a (Scheme. tv #{}))
         [s t]   (infer _Gamma e)
         [p1 t1] (release t)
         [p2 t2] (release (t/substitute tv s))
         pred    (distinct (remove nil? (flatten [p1 p2])))
         t       (Arrow. t2 t1)]
     [s (if (seq pred) (Predicated. pred t) t)]))

  ([_Gamma [Apply e1 e2]]
    (let [tv      (Var. (gensym 'ξ))
          [s1 t1] (infer _Gamma e1)
          [s2 t2] (infer (t/substitute _Gamma s1) e2)
          [p1 t1] (release t1)
          [p2 t2] (release t2)
          _ (throw (Exception. "Here, you can really release!"))
          ;; probably not needed in unify
          s3      (unify _Gamma (t/substitute t1 s2) (Arrow. t2 tv))
          [p3 t3] (release (t/substitute tv s3))
          pred    (distinct(remove nil? (flatten [p1 p2 p3])))]
      [(compose _Gamma s3 s2 s1)
       (if (seq pred) (Predicated. (t/substitute pred s3) t3) t3)])))

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
