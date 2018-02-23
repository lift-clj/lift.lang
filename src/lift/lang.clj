(ns lift.lang
  (:refer-clojure :exclude [+ * - / case defn let map name read = not=])
  (:require
   [lift.lang.case :as case]
   [lift.lang.defn :as defn]
   [lift.lang.interface :as iface]
   [lift.lang.let :as let]
   [lift.lang.monad :as monad]
   [lift.lang.prim :as prim]
   [lift.lang.type :as type]
   [lift.lang.type.data :as data]))

(type/def List      (List a))
(type/def Vector    (Vector a))
(type/def not       (Boolean -> Boolean))
(type/def instance? (Class -> a -> Boolean))
(type/def inc       (Long -> Long))
(type/def pos?      (Long -> Boolean))
(type/def not       (Boolean -> Boolean))
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

(defmacro with-ctor
  {:style/indent :defn}
  [data-decl type-sig ctor-fn]
  (data/private-data* type-sig ctor-fn (rest data-decl)))

(defmacro interface
  {:style/indent :defn}
  [type & decl]
  (iface/interface type decl))

(defmacro impl
  {:style/indent [:defn [:defn]]}
  [type & impls]
  (iface/impl type impls))

(defmacro case [x & pattern-exprs]
  (case/case* x pattern-exprs))

(defmacro defn [name & decl]
  (defn/defn* name decl))

(defmacro let [bindings expr]
  (let/destructuring-let bindings expr))

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

(impl (Eq Long)
  (= [x y] (prim/=Long x y)))

(impl (Eq Character)
  (= [x y] (prim/=Character x y)))

(impl (Eq String)
  (= [x y] (prim/=String x y)))

(impl (Eq Maybe)
  (=
    ([(Just x) (Just y)] (= x y))
    ([Nothing   Nothing] True)
    ([_         _      ] False)))

(impl (Eq Pair)
  (=
    ([(Pair a b) (Pair c d)]
     (if (= a c) (= b d) False))))

;; (impl (Eq Pair)
;;   (= [x y]
;;     (case [x y]
;;       [(Pair a b) (Pair c d)] (if (= a c) (= b d) False))))

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

(interface (Div a)
  (/ (a -> a -> Double)))

(impl (Div Long)
  (/ [x y] (prim/divLong x y)))

(impl (Div Double)
  (/ [x y] (prim/divDouble x y)))

(interface (Functor f)
  (map ((a -> b) -> f a -> f b)))

(impl (Functor Maybe)
  (map
    ([f (Just a)] (Just (f a)))
    ([f  Nothing]  Nothing)))

(impl (Functor List)
  (map [f xs] (prim/mapList f xs)))

(impl (Functor Vector)
  (map [f xs] (prim/mapVector f xs)))

(interface (Monad m)
  (return (a -> m a))
  (>>=    (m a -> (a -> m b) -> m b))
  (>>     (m a -> m b -> m b)))

(impl (Monad Maybe)
  (return
    ([a] (Just a)))
  (>>=
    ([(Just x) f] (f x))
    ([Nothing  _] Nothing)))

;; (with-ctor
;;   (data Email = Email String)
;;   (String -> Maybe Email)
;;   (fn [s]
;;     (if (.contains s "@") (Just (Email s)) Nothing)))

;; (data State s a = State h)

;; (impl (Monad State)
;;   (return
;;     ([a] (State (fn [s] (case/Tuple2 a s)))))
;;   (>>=
;;     ([(State h) f]
;;      (State (fn [s]
;;               (let [[a s']    (h s)
;;                     (State g) (f a)]
;;                 (g s')))))))

;; TODO: State monad needs:
;; record function type constructor syntax
