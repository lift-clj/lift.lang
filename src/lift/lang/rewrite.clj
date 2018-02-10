(ns lift.lang.rewrite
  (:require
   [lift.f.functor :as f]
   [lift.lang.inference :refer [infer]]
   [lift.lang.pattern :as p]
   [lift.lang.type :as t]
   [lift.lang.util :as u]
   [lift.lang.type.impl :as impl]
   [lift.lang.analyze :as ana]))

(t/import-syntax-types)
(t/import-type-types)

(declare rewrite)

(extend-protocol f/Functor
  clojure.lang.Fn (-map [x f] x))

(p/defn -rewrite
  ([_Gamma [SyntaxNode
       [Symbol f]
       [Predicated [[Predicate _ [Const _] :as p]] [Arrow :as t]]]]
   (-> _Gamma (get p) (get (u/resolve-sym f)) (->> (rewrite _Gamma))))
  ([_ [SyntaxNode [Apply [Symbol :as e1] e2] [Arrow _]]]
   (Apply. (Curry. e1) e2))
  ([_ [SyntaxNode x _]] x)
  ([_ [Curry f]] f)
  ([_ x] x))

(defn rewrite [_Gamma x]
  (impl/cata (fn rewrite [x] (-rewrite _Gamma x)) x))

(defn ctor [s]
  (let [n (name s)]
    (when (.endsWith n ".")
      `#(new ~(resolve (symbol (subs n 0 (dec (count n))))) %))))

(p/defn -emit
  ([[Literal a]] a)
  ([[Symbol a]] a)
  ([[Lambda [Symbol x] e]] `(fn* [~x] ~e))
  ([[Apply e1 e2]] (list e1 e2))
  ([[Curry f]] `(fn* [x#] (partial ~f x#)))
  ([expr]
   (throw
    (Exception. (format "Unrecognized expr %s : %s"
                        (pr-str expr)
                        (pr-str (type expr)))))))

(defn emit [expr]
  (impl/cata -emit expr))
