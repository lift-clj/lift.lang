(ns lift.lang
  (:refer-clojure :exclude [case read = not=])
  (:require
   [lift.lang.interface :as iface]
   [lift.lang.prim :as prim]
   [lift.lang.type :as type]
   [lift.lang.type.data :as data]))

(defmacro data
  {:style/indent :defn}
  [& decl]
  (data/data decl))

(defmacro interface
  {:style/indent :defn}
  [type & decl]
  (iface/interface type decl))

(defmacro impl
  {:style/indent :defn}
  [type & impls]
  (iface/impl type impls))

(defmacro case [x & exprs]
  (type/case x exprs))

(data Boolean = True | False)
(data Maybe a = Just a | Nothing)
(data Either a b = Left a | Right b)
(data Pair a b = Pair a b)

(type/def not (Boolean -> Boolean))
(type/def instance? (Class -> a -> Boolean))

(interface (Eq a)
  (=    (a -> a -> Boolean))
  (not= (a -> a -> Boolean))
  (default
   (=    [x y] (not (not= x y)))
   (not= [x y] (not (= x y)))))

;; (impl (Eq Long)
;;   (= [x y] (prim/=Long x y)))

;; (impl (Eq Character)
;;   (= [x y] (prim/=Character x y)))

;; ;; (impl (Eq Maybe)
;; ;;   (=
;; ;;    ([[Just x] [Just y]] (= x y))
;; ;;    ([Nothing   Nothing] True)
;; ;;    ([_         _      ] False)))

;; (interface (Read a)
;;   (read (String -> a)))

;; (impl (Read Long)
;;   (read [s] (prim/readLong s)))

;; (impl (Read Character)
;;   (read [s] (prim/readCharacter s)))

;; (interface (Coercible a b)
;;   (coerce (a -> b)))

;; (impl (Coercible String Long)
;;   (coerce [a] (read a)))
