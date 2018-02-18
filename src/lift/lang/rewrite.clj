(ns lift.lang.rewrite
  (:require
   [lift.f.functor :as f]
   [lift.lang.inference :as infer :refer [infer]]
   [lift.lang.pattern :as p]
   [lift.lang.type.base :as base]
   [lift.lang.util :as u]
   [lift.lang.type.impl :as impl]
   [lift.lang.analyze :as ana])
  (:import
   [clojure.lang Fn IPersistentMap]))

(base/import-container-types)
(base/import-syntax-types)
(base/import-type-types)

(declare rewrite)

(extend-protocol f/Functor
  clojure.lang.Fn (-map [x f] x))

(p/defn -rewrite
  ([_Gamma sub [SyntaxNode
       [Symbol f]
       [Predicated [[Predicate _ as :as p]] [Arrow :as t]] :as syn]]
   (if (infer/concrete-instance? p)
     (let [p (infer/concrete-instance p)]
       (-> _Gamma (get p) (get (u/resolve-sym f)) (->> (rewrite _Gamma sub))))
     (Curry. (resolve f))))
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
  ([[Map r]] r)
  ([[Prim f]] f)
  ([[Curry f]] `(fn* [x#] (partial ~f x#)))
  ([expr]
   (if (var? expr)
     expr
     (throw
      (Exception. (format "Unrecognized expr %s : %s"
                          (pr-str expr)
                          (pr-str (type expr))))))))

(defn emit [expr]
  (impl/cata -emit expr))
