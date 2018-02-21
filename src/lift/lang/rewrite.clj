(ns lift.lang.rewrite
  (:require
   [lift.f.functor :as f]
   [lift.lang.inference :as infer :refer [infer]]
   [lift.lang.pattern :as p]
   [lift.lang.type.base :as base]
   [lift.lang.util :as u]
   [lift.lang.type.impl :as impl]
   [lift.lang.analyze :as ana]
   [lift.lang.type :as type]
   [lift.lang.unification :as unify])
  (:import
   [clojure.lang Fn IPersistentMap]
   [lift.lang.inference InferError]))

(base/import-container-types)
(base/import-syntax-types)
(base/import-type-types)

(declare rewrite)

(extend-protocol f/Functor
  clojure.lang.Fn (-map [x f] x))

;; (get @type/type-env 'Maybe)
;; (get @type/type-env 'Eq)

;; (unify/unify (Container. 'Maybe [(Var. 'a)])
;;              (Container. 'Maybe [(Const. 'Long)]))

(p/defn unify-predicate
  ([[Predicate t a] [Predicate t' a']]
   (if (and (= t t') (= (count a) (count a')))
     (if (= a a')
       type/id
       (->> (map (fn [a a'] [(:a a') a]) a a')
            (into {})
            (type/sub)))
     type/id))
  ([_ _] type/id))

(p/defn -rewrite
  ([_Gamma sub [SyntaxNode
           [Symbol f]
           [Predicated [[Predicate ptag as :as p]] [Arrow :as t]] :as syn]]
   (if (infer/concrete-instance? p)
     (let [[_ as' :as inst] (infer/concrete-instance p)
           [sub'] (some->> (map (fn [a a']
                                   (when-not (instance? Const a)
                                     (unify/unify a (:t (type/find-type _Gamma (:x a'))))))
                                 as as')
                           (remove nil?)
                           (seq)
                           (reduce unify/compose))
           sub' (if (seq sub') (type/sub (merge (:s sub) sub')) sub)
           code (-> (get _Gamma inst)
                    (get (u/resolve-sym f))
                    (type/substitute sub'))]
       (rewrite _Gamma sub' code))
     (let [[m :as psub] (unify-predicate p (type/get-type _Gamma ptag))
           sub' (unify/compose sub psub)
           syn' (type/substitute syn sub')]
       (if (not= syn syn')
         (rewrite _Gamma sub' syn')
         (Curry. (resolve f))))))
  ([_ _ [SyntaxNode [Apply [Lambda :as e1] e2] [Arrow _]]]
   (Apply. e1 e2))
  ([_ _ [SyntaxNode [Apply e1 e2] [Arrow _]]]
   (Apply. (Curry. e1) e2))
  ([_ _ [SyntaxNode x _]] x)
  ([_ _ [Curry f]] f)
  ([_ _ x] x))

(defn rewrite [_Gamma sub x]
  (impl/cata (fn rewrite [x] (-rewrite _Gamma sub x)) x))

(defn ctor [s]
  (let [n (name s)]
    (when (.endsWith n ".")
      `#(new ~(resolve (symbol (subs n 0 (dec (count n))))) %))))

(p/defn -emit
  ([[Literal a]] a)
  ([[Symbol a]] a)
  ([[Key k]] k)
  ([[Lambda [Symbol x] e]] `(fn* [~x] ~e))
  ([[Apply e1 e2]] (list e1 e2))
  ([[Let x e1 e2]] (list 'let [x e1] e2))
  ([[If cond then else]] (list 'if cond then else))
  ([[Select l r]] (list r l))
  ([[Restrict l r]] (list 'dissoc r l))
  ([[List xs]] `(list ~@xs))
  ([[Vector xs]] xs)
  ([[Map r]] r)
  ([[Prim f]] f)
  ([[Curry f]] `(fn* [x#] (partial ~f x#)))
  ([[InferError e m]] `(InferError. ~e ~m))
  ([expr]
   (if (var? expr)
     expr
     (throw
      (Exception. (format "Unrecognized expr %s : %s"
                          (pr-str expr)
                          (pr-str (type expr))))))))

(defn emit [expr]
  (impl/cata -emit expr))
