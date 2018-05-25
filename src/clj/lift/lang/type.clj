(ns lift.lang.type
  (:require
   [clojure.set :refer [union]]
   [lift.lang.type.data :as data :refer [data prim]]))

;;; Bootstrapping

(prim TCon a)
(prim TVar a)
(prim TArr a b)
(prim TApp f x)
(prim Prim f)
(prim Forall as t)

(defn ftv [x]
  (case (data/tag x)
    TCon #{}
    TVar #{(nth x 0 nil)}
    TArr (union (ftv (nth x 0 nil)) (ftv (nth x 1 nil)))
    TApp (union (ftv (nth x 0 nil)) (ftv (nth x 1 nil)))))

;;; Type definition

(data Type
  = TCon Symbol     ; Long
  | TVar Symbol     ; a
  | TArr Type Type  ; a -> b
  | TApp Type Type) ; Maybe a

(data Literal
  = Int Integer
  | Str String)

(data Expr
  = Lit Literal
  | Sym Symbol
  | App Expr Expr
  | Lam Symbol Expr)

(data Tuple1 a = Tuple1 a)
(data Tuple2 a b = Tuple2 a b)
(data Tuple3 a b c = Tuple3 a b c)
(data Tuple4 a b c d = Tuple4 a b c d)
(data Tuple5 a b c d e = Tuple5 a b c d e)
(data Tuple6 a b c d e f = Tuple6 a b c d e f)
(data Tuple7 a b c d e f g = Tuple7 a b c d e f g)
(data Tuple8 a b c d e f g h = Tuple8 a b c d e f g h)
(data Tuple9 a b c d e f g h i = Tuple9 a b c d e f g h i)
