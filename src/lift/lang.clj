(ns lift.lang
  (:refer-clojure :exclude [+ * - / case defn name read = not=])
  (:require
   [lift.lang.case :as case]
   [lift.lang.defn :as defn]
   [lift.lang.interface :as iface]
   [lift.lang.prim :as prim]
   [lift.lang.type :as type]
   [lift.lang.type.data :as data]))

(type/def not       (Boolean -> Boolean))
(type/def instance? (Class -> a -> Boolean))
(type/def inc       (Long -> Long))
(type/def pos?      (Long -> Boolean))
(type/def str       (a -> String))
;; (type/def nth       (Vector a -> Long -> a))
(type/def double    (Ratio -> Double))
(type/def list      (List a))
(type/def cons      (a -> (List a) -> (List a)))
(type/def first     ((List a) -> a))
(type/def vector    (Vector a))
(type/def conj      (Vector a -> a -> Vector a))
(type/def map.      {})
(type/def get       ({l a | r} -> l -> a))
(type/def assoc     ({| r} -> l -> a -> {l a | r}))
(type/def dissoc    ({l a | r} -> l ->  {| r}))
(type/def keyword   (String -> Keyword))
(type/def reverse   (List a -> List a))
(type/def map       ((a -> b) -> (List a) -> (List b)))

(defmacro data
  {:style/indent :defn}
  [& decl]
  (data/data* decl))

(defmacro interface
  {:style/indent :defn}
  [type & decl]
  (iface/interface type decl))

(defmacro impl
  {:style/indent [:defn [1]]}
  [type & impls]
  (iface/impl type impls))

(defmacro case [x & pattern-exprs]
  (case/case* x pattern-exprs))

(defmacro defn [name & decl]
  (defn/defn* name decl))

(data Boolean = True | False)
(data Maybe a = Just a | Nothing)
(data Either a b = Left a | Right b)
(data Pair a b = Pair a b)

(interface (Eq a)
  (=    (a -> a -> Boolean))
  (not= (a -> a -> Boolean))
  (default
   (=    [x y] (not (not= x y)))
   (not= [x y] (not (= x y)))))

;; (fn* needs to be added to syntax parsing)
(impl (Eq Long)
  (= [x y] (prim/=Long x y)))

(impl (Eq Character)
  (= [x y] (prim/=Character x y)))

(impl (Eq String)
  (= [x y] (prim/=String x y)))

;; ;; (fn* needs to be added to syntax parsing)
;; (impl (Eq Maybe)
;;   (= [x y]
;;     (case [x y]
;;       [(Just x) (Just y)] (= x y)
;;       [Nothing   Nothing] True
;;       [_         _      ] False)))

;; TODO: Eq Maybe is correct, Eq (Maybe a) is odd

(interface (Read a)
  (read (String -> a)))

(impl (Read Long)
  (read [s] (prim/readLong s)))

(impl (Read Character)
  (read [s] (prim/readCharacter s)))

(interface (Coercible a b)
  (coerce (a -> b)))

(impl (Coercible String Long)
  (coerce [a] (read a)))

(interface (Named a)
  (name (a -> String)))

(impl (Named Keyword)
  (name [a] (prim/nameKeyword a)))

(impl (Named Symbol)
  (name [a] (prim/nameSymbol a)))

(interface (Num a)
  (+ (a -> a -> a))
  (* (a -> a -> a))
  (- (a -> a -> a)))

(impl (Num Long)
  (+ [x y] (prim/+Long x y))
  (* [x y] (prim/*Long x y))
  (- [x y] (prim/-Long x y)))

(impl (Num Double)
  (+ [x y] (prim/+Double x y))
  (* [x y] (prim/*Double x y))
  (- [x y] (prim/-Double x y)))

(interface (Div a b)
  (/ (a -> a -> b)))

(impl (Div Long Ratio)
  (/ [x y] (prim/divLong x y)))

(impl (Div Double Double)
  (/ [x y] (prim/divDouble x y)))
