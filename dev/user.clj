(ns user
  (:refer-clojure :exclude [= not= read])
  (:require
   [clojure.core :as c]
   [clojure.pprint :refer [pprint]]
   [clojure.tools.namespace.repl :refer [refresh]]
   [lift.lang.analyze :as ana]
   [lift.lang.inference :refer [check -infer infer]]
   [lift.lang.interface :refer [interface impl]]
   [prelude :refer :all]
   [primitives :as prim]
   [lift.lang.rewrite :refer [-emit emit -rewrite rewrite]]
   [lift.lang.signatures :as sig]
   [lift.lang.type :as t :refer [id import-syntax-types import-type-types]]
   [lift.lang.type.impl :as impl]
   ))


(defn print-type-env []
  (pprint @t/type-env))

(import-syntax-types)
(import-type-types)

(interface (Eq a)
  (=    (a -> a -> Boolean))
  (not= (a -> a -> Boolean))
  (default
   (=    [x y] (c/= x y))
   (not= [x y] (not (= x y)))))

(impl (Eq Long)
  (=    [x y] (prim/=Long x y))
  (not= [x y] (not (= x y))))

(impl (Eq Character)
  (=    [x y] (prim/=Character x y)))


;; (swap! t/type-env dissoc (Predicate. 'Eq (Const. 'Long)))
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

(check '(= 1 (coerce "1")))

;; ;; ;; ;; ;; c/eval
;; ;; ;; ;; eval
;; ;; ;; eval
;; ;; ;; emit
;; eval
;; (emit
;;  (impl/cata
;;   (partial -rewrite @t/type-env)
;;   (check '(= 1 (read "1")))))

;; ;; eval
;; ;; emit
;; eval
;; emit
;; impl/cata
;; (partial -rewrite @t/type-env)
;; (check '(not= \2 (read "\\2")))
